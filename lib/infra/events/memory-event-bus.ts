import type { DomainEvent, EventBus, EventHandler } from '@/lib/platform/events/types';

// In-memory event bus for development.
// Replace with Kafka/Redis/Service Bus adapter for production.

type Subscription = {
  eventType: string | '*';
  handler: EventHandler;
};

class InMemoryEventBus implements EventBus {
  private subscriptions: Subscription[] = [];
  private eventLog: DomainEvent[] = [];
  private readonly maxLogSize = 10000;

  async publish<T>(event: DomainEvent<T>): Promise<void> {
    // Store in event log (for replay/debugging)
    this.eventLog.push(event as DomainEvent);
    if (this.eventLog.length > this.maxLogSize) {
      this.eventLog = this.eventLog.slice(-this.maxLogSize / 2);
    }

    // Dispatch to subscribers
    const handlers = this.subscriptions.filter(
      (s) => s.eventType === '*' || s.eventType === event.eventType
    );

    await Promise.all(
      handlers.map(async (s) => {
        try {
          await s.handler(event as DomainEvent);
        } catch (err) {
          console.error(`[EventBus] Handler error for ${event.eventType}:`, err);
        }
      })
    );
  }

  subscribe<T>(eventType: string, handler: EventHandler<T>): () => void {
    const subscription: Subscription = {
      eventType,
      handler: handler as EventHandler,
    };
    this.subscriptions.push(subscription);

    return () => {
      const idx = this.subscriptions.indexOf(subscription);
      if (idx >= 0) this.subscriptions.splice(idx, 1);
    };
  }

  subscribeAll(handler: EventHandler): () => void {
    return this.subscribe('*', handler);
  }

  // Dev helper: get recent events for debugging
  getRecentEvents(limit = 100): DomainEvent[] {
    return this.eventLog.slice(-limit);
  }

  // Dev helper: get events since a cursor (for catch-up)
  getEventsSince(cursor: string, limit = 100): { events: DomainEvent[]; nextCursor: string } {
    const cursorTime = cursor ? new Date(cursor).getTime() : 0;
    const events = this.eventLog
      .filter((e) => new Date(e.occurredAt).getTime() > cursorTime)
      .slice(0, limit);

    const nextCursor = events.length > 0 
      ? events[events.length - 1].occurredAt 
      : cursor || new Date().toISOString();

    return { events, nextCursor };
  }
}

// Singleton instance
export const inMemoryEventBus = new InMemoryEventBus();

// Default export for easy swapping
export const eventBus: EventBus = inMemoryEventBus;
