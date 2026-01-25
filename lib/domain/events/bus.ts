// ============================================================================
// EVENT BUS - Simple pub/sub system for domain events
// ============================================================================

export interface EventBus {
  emit: <T>(eventName: string, payload: T) => void;
  on: <T>(eventName: string, handler: (payload: T) => void) => () => void;
  off: <T>(eventName: string, handler: (payload: T) => void) => void;
}

type EventHandler = (payload: unknown) => void;

class SimpleEventBus implements EventBus {
  private handlers = new Map<string, Set<EventHandler>>();

  emit<T>(eventName: string, payload: T): void {
    const eventHandlers = this.handlers.get(eventName);
    if (eventHandlers) {
      eventHandlers.forEach((handler) => {
        try {
          handler(payload);
        } catch (error) {
          console.error(`Error in event handler for ${eventName}:`, error);
        }
      });
    }
  }

  on<T>(eventName: string, handler: (payload: T) => void): () => void {
    if (!this.handlers.has(eventName)) {
      this.handlers.set(eventName, new Set());
    }
    this.handlers.get(eventName)!.add(handler as EventHandler);
    
    // Return unsubscribe function
    return () => this.off(eventName, handler);
  }

  off<T>(eventName: string, handler: (payload: T) => void): void {
    const eventHandlers = this.handlers.get(eventName);
    if (eventHandlers) {
      eventHandlers.delete(handler as EventHandler);
    }
  }
}

// Singleton instance
let eventBus: EventBus | null = null;

export function getEventBus(): EventBus {
  if (!eventBus) {
    eventBus = new SimpleEventBus();
  }
  return eventBus;
}
