import type { ApiStoredMessage } from '@/lib/sync';
import { readJson, writeJson } from '@/lib/persist';

type Pending = {
  chatId: string;
  message: ApiStoredMessage;
  addedAt: number;
};

const KEY = 'pending_messages_v1';

export function enqueue(chatId: string, message: ApiStoredMessage) {
  const current = readJson<Pending[]>(KEY) ?? [];
  current.push({ chatId, message, addedAt: Date.now() });
  writeJson(KEY, current);
}

export function peekAll(): Pending[] {
  return readJson<Pending[]>(KEY) ?? [];
}

export function clearAll() {
  writeJson(KEY, [] as Pending[]);
}

export function replaceAll(next: Pending[]) {
  writeJson(KEY, next);
}
