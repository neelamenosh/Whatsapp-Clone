import { promises as fs } from 'node:fs';
import path from 'node:path';

export type StoredMessage = {
  id: string;
  chatId: string;
  senderId: string;
  content: string;
  timestamp: string; // ISO
  status: 'sending' | 'sent' | 'delivered' | 'read';
  type: 'text' | 'image' | 'voice' | 'location' | 'document';
  expiresAt?: string; // ISO
};

type Db = {
  messages: StoredMessage[];
};

const DATA_DIR = path.join(process.cwd(), '.data');
const DB_PATH = path.join(DATA_DIR, 'messages.json');

let writeLock: Promise<void> = Promise.resolve();

async function ensureDb(): Promise<void> {
  await fs.mkdir(DATA_DIR, { recursive: true });
  try {
    await fs.access(DB_PATH);
  } catch {
    const initial: Db = { messages: [] };
    await fs.writeFile(DB_PATH, JSON.stringify(initial, null, 2), 'utf8');
  }
}

async function readDb(): Promise<Db> {
  await ensureDb();
  const raw = await fs.readFile(DB_PATH, 'utf8');
  const parsed = JSON.parse(raw) as Db;
  return { messages: Array.isArray(parsed.messages) ? parsed.messages : [] };
}

async function writeDb(db: Db): Promise<void> {
  await ensureDb();
  await fs.writeFile(DB_PATH, JSON.stringify(db, null, 2), 'utf8');
}

function withLock<T>(fn: () => Promise<T>): Promise<T> {
  const run = async () => fn();
  const next = writeLock.then(run, run);
  writeLock = next.then(
    () => undefined,
    () => undefined
  );
  return next;
}

export async function listMessages(chatId: string, sinceMs?: number): Promise<StoredMessage[]> {
  const db = await readDb();
  const sinceIso = typeof sinceMs === 'number' ? new Date(sinceMs).toISOString() : null;
  const now = Date.now();

  return db.messages
    .filter((m) => m.chatId === chatId)
    .filter((m) => {
      if (!m.expiresAt) return true;
      return new Date(m.expiresAt).getTime() > now;
    })
    .filter((m) => {
      if (!sinceIso) return true;
      return m.timestamp > sinceIso;
    })
    .sort((a, b) => a.timestamp.localeCompare(b.timestamp));
}

export async function appendMessage(message: StoredMessage): Promise<StoredMessage> {
  return withLock(async () => {
    const db = await readDb();
    db.messages.push(message);
    await writeDb(db);
    return message;
  });
}
