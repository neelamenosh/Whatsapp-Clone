import type { IdempotencyStore, IdempotencyRecord } from '@/lib/domain/messaging/idempotency';

// In-memory idempotency store for dev/demo.
// In production, use Redis or a database table with TTL.

const store = new Map<string, IdempotencyRecord>();

// Default TTL: 24 hours
const DEFAULT_TTL_MS = 24 * 60 * 60 * 1000;

export const memoryIdempotencyStore: IdempotencyStore = {
  async get(key: string): Promise<IdempotencyRecord | null> {
    const record = store.get(key);
    if (!record) return null;

    // Check TTL
    const age = Date.now() - new Date(record.createdAt).getTime();
    if (age > DEFAULT_TTL_MS) {
      store.delete(key);
      return null;
    }

    return record;
  },

  async set(record: IdempotencyRecord): Promise<boolean> {
    // Check if already exists (and not expired)
    const existing = await this.get(record.key);
    if (existing) {
      return false;
    }

    store.set(record.key, record);
    return true;
  },

  async prune(olderThanMs: number): Promise<number> {
    const cutoff = Date.now() - olderThanMs;
    let pruned = 0;

    for (const [key, record] of store.entries()) {
      if (new Date(record.createdAt).getTime() < cutoff) {
        store.delete(key);
        pruned++;
      }
    }

    return pruned;
  },
};
