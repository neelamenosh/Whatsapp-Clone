import type { MessageDTO } from '@/lib/contracts/messaging';
import type { DomainEvent } from '@/lib/platform/events/types';
import { createEvent, EventTypes } from '@/lib/platform/events/types';

// Message event payloads

export interface MessageCreatedPayload {
  message: MessageDTO;
  chatId: string;
}

export interface MessageDeliveredPayload {
  messageId: string;
  chatId: string;
  deliveredTo: string;
  deliveredAt: string;
}

export interface MessageReadPayload {
  messageId: string;
  chatId: string;
  readBy: string;
  readAt: string;
}

export interface TypingPayload {
  chatId: string;
  userId: string;
}

export interface PresencePayload {
  userId: string;
  status: 'online' | 'offline' | 'away';
  lastSeen?: string;
}

// Event factory functions

export function messageCreatedEvent(
  message: MessageDTO,
  correlationId?: string,
  tenantId?: string
): DomainEvent<MessageCreatedPayload> {
  return createEvent(
    EventTypes.MESSAGE_CREATED,
    { message, chatId: message.chatId },
    { correlationId, source: 'messaging-service', tenantId: tenantId ?? message.tenantId ?? 'default' }
  );
}

export function messageDeliveredEvent(
  messageId: string,
  chatId: string,
  deliveredTo: string,
  correlationId?: string
): DomainEvent<MessageDeliveredPayload> {
  return createEvent(
    EventTypes.MESSAGE_DELIVERED,
    { messageId, chatId, deliveredTo, deliveredAt: new Date().toISOString() },
    { correlationId, source: 'messaging-service' }
  );
}

export function messageReadEvent(
  messageId: string,
  chatId: string,
  readBy: string,
  correlationId?: string
): DomainEvent<MessageReadPayload> {
  return createEvent(
    EventTypes.MESSAGE_READ,
    { messageId, chatId, readBy, readAt: new Date().toISOString() },
    { correlationId, source: 'messaging-service' }
  );
}

export function typingStartedEvent(
  chatId: string,
  userId: string
): DomainEvent<TypingPayload> {
  return createEvent(
    EventTypes.TYPING_STARTED,
    { chatId, userId },
    { source: 'presence-service' }
  );
}

export function typingStoppedEvent(
  chatId: string,
  userId: string
): DomainEvent<TypingPayload> {
  return createEvent(
    EventTypes.TYPING_STOPPED,
    { chatId, userId },
    { source: 'presence-service' }
  );
}

export function presenceChangedEvent(
  userId: string,
  status: 'online' | 'offline' | 'away',
  lastSeen?: string
): DomainEvent<PresencePayload> {
  return createEvent(
    EventTypes.PRESENCE_CHANGED,
    { userId, status, lastSeen },
    { source: 'presence-service' }
  );
}
