// Domain event types and event bus abstraction.
// In production, implement with Kafka, Azure Service Bus, Redis Streams, etc.

export interface DomainEvent<T = unknown> {
  eventId: string;
  eventType: string;
  occurredAt: string; // ISO timestamp
  schemaVersion: number;
  // Correlation for distributed tracing
  correlationId?: string;
  // Source service/module
  source: string;
  // Multi-tenant support
  tenantId?: string;
  // The event payload
  payload: T;
}

export type EventHandler<T = unknown> = (event: DomainEvent<T>) => Promise<void>;

export interface EventBus {
  // Publish an event to the bus
  publish<T>(event: DomainEvent<T>): Promise<void>;

  // Subscribe to events of a specific type
  subscribe<T>(eventType: string, handler: EventHandler<T>): () => void;

  // Subscribe to all events (for logging, auditing, etc.)
  subscribeAll(handler: EventHandler): () => void;
}

// Event factory helper
export function createEvent<T>(
  eventType: string,
  payload: T,
  options?: {
    schemaVersion?: number;
    correlationId?: string;
    source?: string;
    tenantId?: string;
  }
): DomainEvent<T> {
  return {
    eventId: `evt-${Date.now()}-${Math.random().toString(36).slice(2)}`,
    eventType,
    occurredAt: new Date().toISOString(),
    schemaVersion: options?.schemaVersion ?? 1,
    correlationId: options?.correlationId,
    source: options?.source ?? 'messaging-service',
    tenantId: options?.tenantId,
    payload,
  };
}

// Standard messaging events
export const EventTypes = {
  MESSAGE_CREATED: 'messaging.message.created',
  MESSAGE_DELIVERED: 'messaging.message.delivered',
  MESSAGE_READ: 'messaging.message.read',
  MESSAGE_DELETED: 'messaging.message.deleted',
  TYPING_STARTED: 'messaging.typing.started',
  TYPING_STOPPED: 'messaging.typing.stopped',
  PRESENCE_CHANGED: 'messaging.presence.changed',
} as const;

export type EventType = (typeof EventTypes)[keyof typeof EventTypes];
