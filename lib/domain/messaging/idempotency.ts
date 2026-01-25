// Idempotency store interface for preventing duplicate message processing.

export interface IdempotencyRecord {
  key: string;
  messageId: string;
  createdAt: string; // ISO timestamp
}

export interface IdempotencyStore {
  // Returns existing record if key was already used, null otherwise.
  get(key: string): Promise<IdempotencyRecord | null>;

  // Stores the idempotency record. Returns false if key already exists.
  set(record: IdempotencyRecord): Promise<boolean>;

  // Cleanup old records (optional, for TTL-based expiry).
  prune?(olderThanMs: number): Promise<number>;
}
