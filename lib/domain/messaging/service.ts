import { SendMessageRequestSchema, type MessageDTO } from '@/lib/contracts/messaging';
import type { MessageStore } from './store';
import type { IdempotencyStore, IdempotencyRecord } from './idempotency';
import type { EventBus } from '@/lib/platform/events/types';
import type { AuditStore } from '@/lib/platform/audit/types';
import type { SearchService } from '@/lib/platform/search/service';
import { createAuditEntry } from '@/lib/platform/audit/types';
import { messageCreatedEvent } from './events';

export type ListMessagesQuery = {
  chatId: string;
  sinceMs?: number;
  tenantId?: string;
};

export async function listMessages(store: MessageStore, query: ListMessagesQuery): Promise<MessageDTO[]> {
  return store.listMessages(query.chatId, query.sinceMs);
}

export type SendMessageCommand = {
  chatId: string;
  body: unknown;
  idempotencyKey?: string; // From header or body
  correlationId?: string;  // For distributed tracing
  tenantId?: string;       // Multi-tenant support
  actorUserId?: string;    // For audit logging
};

export type SendMessageResult = {
  message: MessageDTO;
  isDuplicate: boolean; // True if this was a replay of an existing idempotency key
};

export type MessagingDependencies = {
  messageStore: MessageStore;
  idempotencyStore: IdempotencyStore | null;
  eventBus: EventBus | null;
  auditStore: AuditStore | null;
  searchService: SearchService | null;
};

export async function sendMessage(
  store: MessageStore,
  idempotencyStore: IdempotencyStore | null,
  command: SendMessageCommand,
  eventBus?: EventBus | null,
  auditStore?: AuditStore | null,
  searchService?: SearchService | null
): Promise<SendMessageResult> {
  const parsed = SendMessageRequestSchema.safeParse(command.body);
  if (!parsed.success) {
    const error = new Error('Invalid message payload');
    (error as any).statusCode = 400;
    (error as any).details = parsed.error.flatten();
    throw error;
  }

  const body = parsed.data;

  // Generate canonical message ID
  const messageId = body.id ?? `m-${Date.now()}-${Math.random().toString(16).slice(2)}`;

  // Derive idempotency key: prefer explicit key, then body.idempotencyKey, then messageId
  const idempotencyKey = command.idempotencyKey ?? body.idempotencyKey ?? messageId;

  // Check idempotency (if store provided)
  if (idempotencyStore) {
    const existing = await idempotencyStore.get(idempotencyKey);
    if (existing) {
      // Return the previously created message (safe replay)
      const existingMessages = await store.listMessages(command.chatId);
      const existingMessage = existingMessages.find((m) => m.id === existing.messageId);
      if (existingMessage) {
        return { message: existingMessage, isDuplicate: true };
      }
      // Message was deleted or not found; proceed as new (edge case)
    }
  }

  // Server assigns authoritative timestamp for ordering
  const serverTimestamp = new Date().toISOString();
  const tenantId = command.tenantId ?? body.tenantId ?? 'default';

  const message: MessageDTO = {
    id: messageId,
    chatId: body.chatId ?? command.chatId,
    senderId: body.senderId,
    content: body.content,
    timestamp: serverTimestamp, // Server-assigned, not client-provided
    status: body.status ?? 'sent',
    type: body.type ?? 'text',
    expiresAt: body.expiresAt,
    tenantId,
  };

  const saved = await store.appendMessage(message);

  // Record idempotency key after successful persist
  if (idempotencyStore) {
    const record: IdempotencyRecord = {
      key: idempotencyKey,
      messageId: saved.id,
      createdAt: serverTimestamp,
    };
    await idempotencyStore.set(record);
  }

  // Publish domain event (for realtime fanout, notifications, etc.)
  if (eventBus) {
    const event = messageCreatedEvent(saved, command.correlationId, tenantId);
    await eventBus.publish(event);
    
    // Index for search (async, non-blocking)
    if (searchService) {
      searchService.indexMessage(event).catch((err) => {
        console.error('[SEARCH] Failed to index message:', err);
      });
    }
  }

  // Audit log (async, non-blocking)
  if (auditStore) {
    const auditEntry = createAuditEntry(
      {
        user: {
          userId: command.actorUserId ?? body.senderId,
          tenantId,
        },
        isAuthenticated: true,
        method: 'jwt',
      },
      'message.send',
      { type: 'message', id: saved.id },
      'success',
      { chatId: saved.chatId, messageId: saved.id }
    );
    auditStore.log(auditEntry).catch((err) => {
      console.error('[AUDIT] Failed to log message send:', err);
    });
  }

  return { message: saved, isDuplicate: false };
}
