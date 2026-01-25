// Store configuration - switch implementations via environment variables.
// In production, swap these for Postgres/Redis implementations.

import type { MessageStore } from '@/lib/domain/messaging/store';
import type { IdempotencyStore } from '@/lib/domain/messaging/idempotency';

const STORE_TYPE = process.env.STORE_TYPE ?? 'sqlite'; // 'sqlite' | 'fs' | 'postgres'

let messageStore: MessageStore;
let idempotencyStore: IdempotencyStore;

if (STORE_TYPE === 'sqlite') {
  // SQLite - recommended for dev/staging (real SQL, good performance)
  const { sqliteMessageStore } = require('./message-store/sqlite-message-store');
  const { sqliteIdempotencyStore } = require('./idempotency/sqlite-idempotency-store');
  messageStore = sqliteMessageStore;
  idempotencyStore = sqliteIdempotencyStore;
} else if (STORE_TYPE === 'fs') {
  // File-based JSON - legacy demo mode
  const { fsMessageStore } = require('./message-store/fs-message-store');
  const { memoryIdempotencyStore } = require('./idempotency/memory-idempotency-store');
  messageStore = fsMessageStore;
  idempotencyStore = memoryIdempotencyStore;
} else {
  // Future: Postgres, etc.
  throw new Error(`Unknown STORE_TYPE: ${STORE_TYPE}. Supported: sqlite, fs`);
}

export { messageStore, idempotencyStore };
