import { NextResponse } from 'next/server';
import { messageStore, idempotencyStore } from '@/lib/infra/stores';
import { eventBus } from '@/lib/infra/events';
import { listMessages, sendMessage } from '@/lib/domain/messaging/service';
import { initRealtimeGateway } from '@/lib/infra/realtime/gateway';
import { extractAuthContext, AuthError } from '@/lib/platform/auth';
import {
  demoChatAuthService,
  authorizeSendMessage,
  authorizeReadMessages,
} from '@/lib/domain/chat/authorization';
import { getAuditStore } from '@/lib/infra/audit/memory-audit-store';
import { getSearchService } from '@/lib/platform/search/service';
import { extractTenantContext } from '@/lib/platform/tenant/context';

export const runtime = 'nodejs';

// Initialize realtime gateway (subscribes to event bus)
// This bridges domain events to WebSocket connections
let gatewayInitialized = false;
if (!gatewayInitialized) {
  initRealtimeGateway(eventBus);
  gatewayInitialized = true;
}

// Get enterprise services (lazy-initialized singletons)
const auditStore = getAuditStore();
const searchService = getSearchService();

// Standard idempotency key header (enterprise pattern)
const IDEMPOTENCY_KEY_HEADER = 'idempotency-key';
// Correlation ID header for distributed tracing
const CORRELATION_ID_HEADER = 'x-correlation-id';

// Feature flag: enable auth enforcement (set to true for production)
const ENFORCE_AUTH = process.env.ENFORCE_AUTH === 'true';

function json(data: unknown, init?: ResponseInit) {
  return NextResponse.json(data, init);
}

function handleError(err: unknown) {
  if (err instanceof AuthError) {
    return json({ error: err.message, code: err.code }, { status: err.statusCode });
  }
  const statusCode = typeof (err as any)?.statusCode === 'number' ? (err as any).statusCode : 500;
  const details = (err as any)?.details;
  const message = statusCode === 400 ? 'Invalid payload' : 'Server error';
  return json({ error: message, details }, { status: statusCode });
}

export async function GET(
  req: Request,
  { params }: { params: { chatId: string } }
) {
  const { chatId } = params;

  // Extract auth context
  const auth = await extractAuthContext(req);

  // Authorize (if enforcement enabled)
  if (ENFORCE_AUTH) {
    try {
      await authorizeReadMessages(demoChatAuthService, auth, chatId);
    } catch (err) {
      return handleError(err);
    }
  }

  const url = new URL(req.url);
  const since = url.searchParams.get('since');
  const sinceMs = since ? Number(since) : undefined;

  const messages = await listMessages(messageStore, {
    chatId,
    sinceMs: Number.isFinite(sinceMs) ? sinceMs : undefined,
  });
  return json({ messages });
}

export async function POST(
  req: Request,
  { params }: { params: { chatId: string } }
) {
  const { chatId } = params;

  // Extract auth context
  const auth = await extractAuthContext(req);

  // Authorize (if enforcement enabled)
  if (ENFORCE_AUTH) {
    try {
      await authorizeSendMessage(demoChatAuthService, auth, chatId);
    } catch (err) {
      return handleError(err);
    }
  }

  // Extract idempotency key and correlation ID from headers
  const idempotencyKey = req.headers.get(IDEMPOTENCY_KEY_HEADER) ?? undefined;
  const correlationId = req.headers.get(CORRELATION_ID_HEADER) ?? `req-${Date.now()}`;

  let body: unknown;
  try {
    body = await req.json();
  } catch {
    return json({ error: 'Invalid JSON' }, { status: 400 });
  }

  try {
    // Extract tenant context from auth
    const tenant = extractTenantContext(auth);
    
    // sendMessage now publishes to event bus, which triggers realtime gateway
    const result = await sendMessage(
      messageStore,
      idempotencyStore,
      { 
        chatId, 
        body, 
        idempotencyKey, 
        correlationId,
        tenantId: tenant.tenantId,
        actorUserId: auth.user?.sub, // 'sub' is the userId claim
      },
      eventBus, // Event bus handles fanout; skips publish internally for duplicates
      auditStore,
      searchService
    );

    // Return 200 for duplicate (idempotent), 201 for new
    const status = result.isDuplicate ? 200 : 201;
    return json({ message: result.message, isDuplicate: result.isDuplicate }, { status });
  } catch (err) {
    return handleError(err);
  }
}
