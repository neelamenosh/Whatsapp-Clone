// Realtime gateway that bridges event bus to WebSocket connections.
// Consumes domain events and fans out to subscribed clients.

import type { DomainEvent } from '@/lib/platform/events/types';
import { EventTypes } from '@/lib/platform/events/types';
import type { MessageCreatedPayload, TypingPayload, PresencePayload } from '@/lib/domain/messaging/events';

const WS_INTERNAL_URL = process.env.WS_INTERNAL_URL ?? 'http://localhost:3001';

/**
 * Publish to realtime gateway (HTTP endpoint on WS server).
 */
async function publishToWs(chatId: string, payload: unknown): Promise<void> {
  try {
    const res = await fetch(`${WS_INTERNAL_URL}/publish`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ chatId, payload }),
    });
    if (!res.ok) {
      console.error('[RealtimeGateway] publish failed:', res.status);
    }
  } catch (err) {
    console.error('[RealtimeGateway] publish error:', err);
  }
}

/**
 * Handle domain events and route to appropriate WebSocket rooms.
 */
export async function handleRealtimeEvent(event: DomainEvent): Promise<void> {
  switch (event.eventType) {
    case EventTypes.MESSAGE_CREATED: {
      const payload = event.payload as MessageCreatedPayload;
      await publishToWs(payload.chatId, {
        type: 'message',
        data: payload.message,
        eventId: event.eventId,
        occurredAt: event.occurredAt,
      });
      break;
    }

    case EventTypes.MESSAGE_DELIVERED: {
      const payload = event.payload as { chatId: string; messageId: string; deliveredTo: string };
      await publishToWs(payload.chatId, {
        type: 'delivered',
        messageId: payload.messageId,
        deliveredTo: payload.deliveredTo,
        eventId: event.eventId,
        occurredAt: event.occurredAt,
      });
      break;
    }

    case EventTypes.MESSAGE_READ: {
      const payload = event.payload as { chatId: string; messageId: string; readBy: string };
      await publishToWs(payload.chatId, {
        type: 'read',
        messageId: payload.messageId,
        readBy: payload.readBy,
        eventId: event.eventId,
        occurredAt: event.occurredAt,
      });
      break;
    }

    case EventTypes.TYPING_STARTED: {
      const payload = event.payload as TypingPayload;
      await publishToWs(payload.chatId, {
        type: 'typing_started',
        userId: payload.userId,
        eventId: event.eventId,
      });
      break;
    }

    case EventTypes.TYPING_STOPPED: {
      const payload = event.payload as TypingPayload;
      await publishToWs(payload.chatId, {
        type: 'typing_stopped',
        userId: payload.userId,
        eventId: event.eventId,
      });
      break;
    }

    default:
      // Unknown event type - log and ignore
      console.log(`[RealtimeGateway] Unhandled event: ${event.eventType}`);
  }
}

/**
 * Initialize realtime gateway by subscribing to event bus.
 */
export function initRealtimeGateway(eventBus: { subscribeAll: (handler: (e: DomainEvent) => Promise<void>) => () => void }): () => void {
  console.log('[RealtimeGateway] Initializing...');
  
  const unsubscribe = eventBus.subscribeAll(async (event) => {
    // Only handle messaging-related events
    if (event.eventType.startsWith('messaging.')) {
      await handleRealtimeEvent(event);
    }
  });

  console.log('[RealtimeGateway] Subscribed to event bus');
  return unsubscribe;
}
