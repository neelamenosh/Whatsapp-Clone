import Database from 'better-sqlite3';
import path from 'node:path';
import fs from 'node:fs';
import type { IdempotencyStore, IdempotencyRecord } from '@/lib/domain/messaging/idempotency';

const DATA_DIR = path.join(process.cwd(), '.data');
const DB_PATH = path.join(DATA_DIR, 'messages.db');

// Ensure data directory exists
fs.mkdirSync(DATA_DIR, { recursive: true });

// Use the same database file as messages
const db = new Database(DB_PATH);

// Create idempotency table
db.exec(`
  CREATE TABLE IF NOT EXISTS idempotency_keys (
    key TEXT PRIMARY KEY,
    message_id TEXT NOT NULL,
    created_at TEXT NOT NULL
  );

  CREATE INDEX IF NOT EXISTS idx_idempotency_created_at 
    ON idempotency_keys(created_at);
`);

// Prepared statements
const getStmt = db.prepare(`
  SELECT key, message_id, created_at
  FROM idempotency_keys
  WHERE key = @key
`);

const insertStmt = db.prepare(`
  INSERT OR IGNORE INTO idempotency_keys (key, message_id, created_at)
  VALUES (@key, @messageId, @createdAt)
`);

const pruneStmt = db.prepare(`
  DELETE FROM idempotency_keys
  WHERE created_at < @cutoff
`);

// Default TTL: 24 hours
const DEFAULT_TTL_MS = 24 * 60 * 60 * 1000;

export const sqliteIdempotencyStore: IdempotencyStore = {
  async get(key: string): Promise<IdempotencyRecord | null> {
    const row = getStmt.get({ key }) as any;
    if (!row) return null;

    // Check TTL
    const age = Date.now() - new Date(row.created_at).getTime();
    if (age > DEFAULT_TTL_MS) {
      // Expired; will be cleaned up by prune
      return null;
    }

    return {
      key: row.key,
      messageId: row.message_id,
      createdAt: row.created_at,
    };
  },

  async set(record: IdempotencyRecord): Promise<boolean> {
    // Check if already exists (and not expired)
    const existing = await this.get(record.key);
    if (existing) {
      return false;
    }

    const result = insertStmt.run({
      key: record.key,
      messageId: record.messageId,
      createdAt: record.createdAt,
    });

    return result.changes > 0;
  },

  async prune(olderThanMs: number): Promise<number> {
    const cutoff = new Date(Date.now() - olderThanMs).toISOString();
    const result = pruneStmt.run({ cutoff });
    return result.changes;
  },
};
