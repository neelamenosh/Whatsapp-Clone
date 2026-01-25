// Event bus configuration - switch implementations via environment.

import type { EventBus } from '@/lib/platform/events/types';

const EVENT_BUS_TYPE = process.env.EVENT_BUS_TYPE ?? 'memory'; // 'memory' | 'redis' | 'kafka'

let eventBus: EventBus;

if (EVENT_BUS_TYPE === 'memory') {
  const { inMemoryEventBus } = require('./memory-event-bus');
  eventBus = inMemoryEventBus;
} else if (EVENT_BUS_TYPE === 'redis') {
  // Future: Redis Streams implementation
  throw new Error('Redis event bus not implemented yet');
} else if (EVENT_BUS_TYPE === 'kafka') {
  // Future: Kafka implementation
  throw new Error('Kafka event bus not implemented yet');
} else {
  throw new Error(`Unknown EVENT_BUS_TYPE: ${EVENT_BUS_TYPE}`);
}

export { eventBus };
