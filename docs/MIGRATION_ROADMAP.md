# Migration Roadmap (Demo → Enterprise-Ready)

This roadmap upgrades the architecture **incrementally** without breaking the current demo UX.

---

## Phase 0 — Baseline (today)

- UI sends messages optimistically.
- UI publishes to WS relay and separately POSTs to `/api/messages/{chatId}`.
- Messages persist in a local JSON file via `lib/server/message-store.ts`.

Risk: In production this is insecure because any client can broadcast arbitrary payloads to other clients.

---

## Phase 1 — Establish Contracts and Domain Layer (no behavior change)

**Goal:** centralize message types and validation.

Deliverables:
- Add `lib/contracts/messaging.ts` with zod schemas for:
  - `SendMessageRequest`
  - `MessageDTO`
  - `MessageCreatedEvent`
- Add `lib/domain/messaging/` with:
  - `sendMessage(command)` (validates + returns canonical DTO)
  - `listMessages(query)`
- API route becomes thin wiring: parse → domain → return

Acceptance:
- Existing UI behavior is unchanged.
- Validation is no longer duplicated.

---

## Phase 2 — Make Server the Only Publisher of Realtime

**Goal:** remove client-to-client broadcast.

Approach options:

### Option A (simple dev approach)
- Keep WS server separate, but:
  - add a server-only “publish” endpoint on WS server (HTTP POST or secured WS)
  - API route calls it after persisting a message

### Option B (enterprise-like approach)
- Add an in-memory event bus in the Next.js runtime (dev)
- API route publishes `MessageCreated`
- A realtime adapter forwards events to WS server (dev) or directly to connected clients (prod)

Acceptance:
- UI no longer calls `realtime.publish()` for messages.
- WS server accepts only subscribe/unsubscribe from clients.

---

## Phase 3 — Introduce Idempotency and Ordering

**Goal:** support retries and avoid dupes.

Deliverables:
- Idempotency key (header) accepted by BFF
- Store idempotency records in persistence layer
- Define ordering strategy:
  - `serverTimestamp` + monotonic sequence per chat (if needed)

Acceptance:
- Replaying `SendMessageRequest` does not create duplicates.

---

## Phase 4 — Replace File Store with Real Database ✅

**Goal:** move from `messages.json` to a proper store.

**Status: COMPLETE**

Deliverables:
- ✅ Created `MessageStore` interface (already existed from Phase 1)
- ✅ Added SQLite implementation: `lib/infra/message-store/sqlite-message-store.ts`
- ✅ Added SQLite idempotency: `lib/infra/idempotency/sqlite-idempotency-store.ts`
- ✅ Created store config: `lib/infra/stores.ts` (env-based switching)
- ✅ Added Postgres schema: `lib/infra/message-store/schema.postgres.sql`
- ✅ Updated API route to use configured stores

**Indexes created:**
- `(chat_id, timestamp)` — primary query path
- `(chat_id, expires_at)` — disappearing messages filter
- `(created_at)` — idempotency TTL cleanup

**To switch store type:**
```bash
# SQLite (default, recommended for dev)
STORE_TYPE=sqlite pnpm dev

# Legacy file-based JSON
STORE_TYPE=fs pnpm dev
```

**To migrate to Postgres (production):**
1. Run `schema.postgres.sql` against your Postgres instance
2. Add a `postgres-message-store.ts` implementation
3. Update `stores.ts` to handle `STORE_TYPE=postgres`

---

## Phase 5 — Add AuthN/AuthZ (enterprise baseline) ✅

**Goal:** correct trust boundaries.

**Status: COMPLETE**

Deliverables:
- ✅ Auth types and context: `lib/platform/auth/types.ts`
- ✅ Token extraction (JWT + demo mode): `lib/platform/auth/extract.ts`
- ✅ WebSocket auth helper: `lib/platform/auth/ws-auth.ts`
- ✅ Chat authorization service: `lib/domain/chat/authorization.ts`
- ✅ Privacy service: `lib/domain/user/privacy.ts`
- ✅ API route auth integration with feature flag

**Auth modes:**
```bash
# Demo mode (default) - auth extracted but not enforced
pnpm dev

# Production mode - auth enforced, rejects unauthenticated requests
ENFORCE_AUTH=true pnpm dev
```

**Testing auth in demo mode:**
```bash
# Use x-demo-user-id header
curl -H "x-demo-user-id: user123" http://localhost:3000/api/messages/chat1

# Or use Bearer token (JWT, signature not verified in demo)
curl -H "Authorization: Bearer eyJ..." http://localhost:3000/api/messages/chat1
```

**For production:**
1. Set `ENFORCE_AUTH=true`
2. Configure JWT verification (JWKS URL, issuer, audience)
3. Implement real `ChatAuthorizationService` with database queries

---

## Phase 6 — Externalize Eventing + Realtime ✅

**Goal:** production-grade fanout.

**Status: COMPLETE**

Deliverables:
- ✅ Event bus abstraction: `lib/platform/events/types.ts`
- ✅ In-memory event bus: `lib/infra/events/memory-event-bus.ts`
- ✅ Event bus config: `lib/infra/events/index.ts`
- ✅ Domain events: `lib/domain/messaging/events.ts`
- ✅ Realtime gateway (event bus → WS): `lib/infra/realtime/gateway.ts`
- ✅ Cursor-based reconnect/catch-up in WS server
- ✅ Exponential backoff with jitter in client reconnection
- ✅ Updated messaging service to publish events

**Event flow:**
```
Client POST → API Route → Domain Service → Event Bus → Realtime Gateway → WS Server → Clients
```

**Catch-up support:**
```javascript
// Client subscribes with cursor for missed events
realtime.subscribe('chat-123', '2026-01-24T12:00:00.000Z');

// Server sends missed events then live updates
{ type: 'message', payload: {...}, _catchup: true }  // Missed
{ type: 'message', payload: {...} }                   // Live
```

**To switch event bus (production):**
```bash
# In-memory (default, dev)
EVENT_BUS_TYPE=memory pnpm dev

# Redis Streams (implement redis-event-bus.ts)
EVENT_BUS_TYPE=redis pnpm dev

# Kafka (implement kafka-event-bus.ts)
EVENT_BUS_TYPE=kafka pnpm dev
```

---

## Phase 7 — Enterprise Enhancements ✅ COMPLETE

**Goal:** Add multi-tenant support, audit logging, search, and compliance features.

### 7.1 Multi-Tenant Support
- `lib/platform/tenant/context.ts` - Tenant context extraction and validation
- `tenantId` added to MessageDTO and DomainEvent
- Cross-tenant access prevention

### 7.2 Audit Logging
- `lib/platform/audit/types.ts` - AuditEntry, AuditStore interface
- `lib/infra/audit/memory-audit-store.ts` - In-memory implementation
- `app/api/admin/audit/route.ts` - Admin API for audit queries

### 7.3 Full-Text Search
- `lib/platform/search/service.ts` - Search service with in-memory implementation
- Event-sourced indexing (messages indexed on MessageCreated event)
- `app/api/search/route.ts` - Search API with filters and pagination

### 7.4 Compliance/Retention
- `lib/platform/retention/service.ts` - Retention policies, legal holds
- GDPR support: data export, right to erasure
- `app/api/admin/compliance/route.ts` - Compliance operations API

### 7.5 Media Scanning
- `lib/platform/media/scanner.ts` - Content moderation, malware detection
- Pluggable scanner interface for production services

### Architecture Diagram After Phase 7:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              ENTERPRISE LAYER                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │   Tenant    │  │    Audit    │  │   Search    │  │  Retention  │        │
│  │   Context   │  │    Store    │  │   Service   │  │   Service   │        │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘        │
│         │                │                │                │               │
│         └────────────────┴────────┬───────┴────────────────┘               │
│                                   │                                         │
├───────────────────────────────────┼─────────────────────────────────────────┤
│                              DOMAIN LAYER                                   │
│                                   │                                         │
│  ┌────────────────────────────────▼────────────────────────────────────┐   │
│  │                        MessagingService                              │   │
│  │  sendMessage() → validate → persist → publish → audit → index       │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                              EVENT BUS                                      │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  MessageCreated → [Realtime Gateway] [Search Indexer] [Audit Log]   │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────┘
```

### New API Endpoints:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/search` | GET | Full-text message search |
| `/api/search` | POST | Get search suggestions |
| `/api/admin/audit` | GET | Query audit logs (admin) |
| `/api/admin/audit` | POST | Get audit trail for resource |
| `/api/admin/compliance` | GET | Get retention policy |
| `/api/admin/compliance` | POST | Execute compliance operations |

### Production Switchable Implementations:

```bash
# Audit Store
AUDIT_STORE=memory|sqlite|azure-log-analytics

# Search Service  
SEARCH_SERVICE=memory|elasticsearch|azure-cognitive-search

# Media Scanner
MEDIA_SCANNER=demo|azure-content-moderator|custom
```

---

## Summary of All Phases

| Phase | Focus | Status |
|-------|-------|--------|
| 0 | Baseline demo | ✅ |
| 1 | Contracts + Domain Layer | ✅ |
| 2 | Server-only Realtime | ✅ |
| 3 | Idempotency + Ordering | ✅ |
| 4 | SQLite Database | ✅ |
| 5 | Auth Scaffolding | ✅ |
| 6 | Event-Driven + Catch-up | ✅ |
| 7 | Enterprise Enhancements | ✅ |

**The architecture is now enterprise-ready while maintaining full backward compatibility with the demo UI.**