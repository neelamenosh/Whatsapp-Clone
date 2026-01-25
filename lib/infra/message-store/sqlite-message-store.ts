import Database from 'better-sqlite3';
import path from 'node:path';
import fs from 'node:fs';
import type { MessageStore } from '@/lib/domain/messaging/store';
import type { MessageDTO } from '@/lib/contracts/messaging';

const DATA_DIR = path.join(process.cwd(), '.data');
const DB_PATH = path.join(DATA_DIR, 'messages.db');

// Ensure data directory exists
fs.mkdirSync(DATA_DIR, { recursive: true });

// Initialize database with schema
const db = new Database(DB_PATH);

// Enable WAL mode for better concurrent read performance
db.pragma('journal_mode = WAL');

// Create messages table with proper indexes
db.exec(`
  CREATE TABLE IF NOT EXISTS messages (
    id TEXT PRIMARY KEY,
    chat_id TEXT NOT NULL,
    sender_id TEXT NOT NULL,
    content TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'sent',
    type TEXT NOT NULL DEFAULT 'text',
    expires_at TEXT,
    created_at TEXT NOT NULL DEFAULT (datetime('now'))
  );

  CREATE INDEX IF NOT EXISTS idx_messages_chat_id_timestamp 
    ON messages(chat_id, timestamp);

  CREATE INDEX IF NOT EXISTS idx_messages_chat_id_expires_at 
    ON messages(chat_id, expires_at) 
    WHERE expires_at IS NOT NULL;
`);

// Prepared statements for performance
const insertStmt = db.prepare(`
  INSERT INTO messages (id, chat_id, sender_id, content, timestamp, status, type, expires_at)
  VALUES (@id, @chatId, @senderId, @content, @timestamp, @status, @type, @expiresAt)
`);

const listStmt = db.prepare(`
  SELECT id, chat_id, sender_id, content, timestamp, status, type, expires_at
  FROM messages
  WHERE chat_id = @chatId
    AND (expires_at IS NULL OR expires_at > @now)
    AND (@sinceIso IS NULL OR timestamp > @sinceIso)
  ORDER BY timestamp ASC
`);

const getByIdStmt = db.prepare(`
  SELECT id, chat_id, sender_id, content, timestamp, status, type, expires_at
  FROM messages
  WHERE id = @id
`);

function rowToDTO(row: any): MessageDTO {
  return {
    id: row.id,
    chatId: row.chat_id,
    senderId: row.sender_id,
    content: row.content,
    timestamp: row.timestamp,
    status: row.status,
    type: row.type,
    expiresAt: row.expires_at ?? undefined,
  };
}

export const sqliteMessageStore: MessageStore = {
  async listMessages(chatId: string, sinceMs?: number): Promise<MessageDTO[]> {
    const now = new Date().toISOString();
    const sinceIso = typeof sinceMs === 'number' ? new Date(sinceMs).toISOString() : null;

    const rows = listStmt.all({ chatId, now, sinceIso });
    return rows.map(rowToDTO);
  },

  async appendMessage(message: MessageDTO): Promise<MessageDTO> {
    insertStmt.run({
      id: message.id,
      chatId: message.chatId,
      senderId: message.senderId,
      content: message.content,
      timestamp: message.timestamp,
      status: message.status,
      type: message.type,
      expiresAt: message.expiresAt ?? null,
    });

    return message;
  },
};

// Export for direct access if needed (e.g., migrations, cleanup jobs)
export { db as sqliteDb };
