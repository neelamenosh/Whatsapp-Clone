# Enterprise Architecture Blueprint (WhatsApp-Clone)

This repo is currently a **single Next.js app + a local WebSocket relay** used for demo/dev:
- UI: `components/**`
- BFF/API (thin): `app/api/messages/[chatId]/route.ts`
- Local WS relay (dev-only): `server/ws-server.mjs`
- Server storage (dev-only): `lib/server/message-store.ts` (JSON file)
- Client persistence/offline: `lib/secure-message-store.ts`, `lib/offline-queue.ts`, `lib/persist.ts`
- Transport adapter (browser): `lib/realtime.ts`

The goal of this document is to define an **enterprise-grade target architecture** and a **clean internal modular structure** you can implement incrementally without losing the demo experience.

---

## 1) Target Logical Architecture (Production)

### 1.1 Main components

1. **Edge / Gateway**
   - TLS termination, WAF, rate limiting, request size limits
   - Auth token verification (OIDC/JWT) and request correlation IDs

2. **BFF (Backend for Frontend)**
   - Client-optimized APIs (REST/GraphQL) and WS session bootstrap
   - Aggregation (chat list + last message + unread counts)
   - Feature flags, request shaping, and client-specific policy enforcement

3. **Domain Services (bounded contexts)**
   - **Messaging Service**: conversations, messages, receipts, membership rules
   - **Realtime Gateway**: fanout to connected clients, reconnect/catch-up
   - **Presence Service**: online/typing, TTL-based ephemeral state
   - **Media Service**: uploads, scanning, signed URLs
   - **Notification Service**: push/email/sms, template routing
   - **Admin/Compliance** (enterprise expansion): audit, retention, legal hold

4. **Data & Integration**
   - Durable stores (messages/conversations/users)
   - Ephemeral store (presence/connections/rate-limits)
   - Object store (media)
   - Event bus/stream (domain events)

### 1.2 How requests flow

**Send message (enterprise pattern):**
1. Client → BFF: `POST /chats/{chatId}/messages` (idempotency key)
2. BFF → Messaging Service: validate membership + persist
3. Messaging Service → Event bus: publish `MessageCreated`
4. Realtime Gateway consumes event and pushes to subscribers
5. Notification Service consumes event for offline devices

**Key requirement:** clients do **not** broadcast arbitrary payloads to other clients. Only server-side domain logic emits canonical events.

---

## 2) Mapping Your Current Repo to the Target

### 2.1 What you have today

- **UI contains domain logic** (send + optimistic state + offline flush) in `components/chat/conversation-view.tsx`.
- **REST persistence** uses `app/api/messages/[chatId]/route.ts` → `lib/server/message-store.ts`.
- **Realtime is a broadcast relay** (`server/ws-server.mjs`) where clients can publish to a room.

### 2.2 What to converge toward (while keeping the demo)

- Treat Next.js routes (`app/api/**`) as the **BFF**.
- Introduce an internal **Messaging module** that owns:
  - message command validation
  - id generation, idempotency
  - persistence contract and event emission
- Convert the WS server from “client-to-client broadcast” into a **server-to-client fanout** target.

In dev, you can keep the WS server as a separate process, but the *server* should be the only publisher.

---

## 3) Recommended Module Boundaries (Inside This Repo)

You can implement an enterprise-ready structure inside a single repo first (a “modular monolith”), then extract services later.

### 3.1 Suggested folders

- `lib/domain/`
  - `messaging/` (commands/queries/events)
  - `presence/` (typing/online policies)
  - `media/` (future)
  - `notifications/` (future)

- `lib/contracts/`
  - Request/response DTOs and event schemas (zod + TS types)
  - Versioning strategy (v1/v2 folders or schema `version` field)

- `lib/platform/`
  - auth (token parsing, scopes)
  - observability (logger/tracing helpers)
  - config (env parsing)
  - errors (typed error model)

- `lib/infra/`
  - `messageStore/` (fs/json today → postgres tomorrow)
  - `eventBus/` (in-memory today → kafka/service bus tomorrow)
  - `realtime/` (ws relay today → managed realtime tomorrow)

- `app/api/**` (BFF)
  - routes are thin: validate input → call domain → return response

### 3.2 Dependency rules (enterprise maintainability)

- `domain` depends only on `contracts` and `platform` (no React, no Next, no fs).
- `infra` implements interfaces defined by `domain`.
- `app/api` depends on `domain` and wires `infra` implementations.
- UI depends only on `contracts` + BFF endpoints; avoid importing server-only modules.

---

## 4) Contracts: Commands, Events, and Validation

### 4.1 Commands (write paths)

- `SendMessageCommand`
  - `chatId`, `senderId`, `content`, `type`, `clientMessageId`, `sentAt`, `expiresAt?`
  - `idempotencyKey` (header or field)

**Responsibilities:**
- Validate shape and constraints (length, allowed types)
- Authorize membership and blocking rules
- Generate canonical `messageId`

### 4.2 Events (async integration)

- `MessageCreated` (canonical)
- `MessageDelivered`, `MessageRead`
- `MessageExpired` (for disappearing messages)

**Event envelope best practices:**
- `eventId`, `eventType`, `occurredAt`, `schemaVersion`, `traceId`
- payload with stable, versioned schema

### 4.3 Schema validation approach

Use **runtime validation** at trust boundaries:
- BFF validates inbound HTTP payloads (zod)
- WS gateway validates inbound control frames (subscribe/unsubscribe)
- Domain validates invariants (membership/blocked/expiry rules)

---

## 5) Security Architecture (What “Enterprise” Means Here)

### 5.1 Authentication
- Use OIDC (Auth0/Entra ID/etc.) and short-lived access tokens
- Validate tokens at edge or BFF; propagate claims via headers/context

### 5.2 Authorization
- Enforce chat membership on every message write and subscription
- Apply privacy settings (blocked users, read receipts, last-seen visibility)

### 5.3 Data protection
- Encryption at rest for server databases + KMS-managed keys
- Client-side encrypted cache (you already have demo AES-GCM) is fine for UX, but is not a substitute for server-side protections

### 5.4 Abuse prevention
- Per-user/per-IP throttles on sends/subscriptions
- Message size limits and upload scanning for media

---

## 6) Scalability & Reliability

### 6.1 Data partitioning
- Partition messages by `chatId` (or by `(tenantId, chatId)` for enterprise multi-tenant)

### 6.2 Realtime fanout
- WS gateway should be stateless with external pub/sub (Redis/Kafka/Service Bus)
- Support reconnect with cursor-based catch-up (`since` tokens)

### 6.3 Outbox pattern (recommended)
- On message persist, write an outbox row/record in the same transaction
- A relay publishes outbox entries to the event bus

This avoids “saved message but no event” failure modes.

---

## 7) Future Expansion Hooks

- **Calls/RTC**: add a signaling bounded context; keep media plane separate (TURN/WebRTC)
- **Search**: build a projection/index from message events (not by querying primary store)
- **Compliance**: retention policies, legal hold, audit trails, export
- **Multi-tenant**: add `tenantId` to every contract early if enterprise is a goal

---

## 8) Concrete Improvements to Align This Repo

These are the highest ROI enterprise-alignment changes:

1. **Single write path**: client posts to BFF; server emits realtime event.
2. **Move message validation to domain** (no more validation spread in UI + routes).
3. **Introduce event bus abstraction** (in-memory in dev, pluggable later).
4. **Add auth scaffolding** (even mock auth) so boundaries are correct.
5. **Add contracts folder** and runtime validation (zod) at boundaries.

See `docs/MIGRATION_ROADMAP.md` for a stepwise plan.
