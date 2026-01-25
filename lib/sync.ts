import type { Message } from '@/lib/types';

export type ApiStoredMessage = {
  id: string;
  chatId: string;
  senderId: string;
  content: string;
  timestamp: string;
  status: Message['status'];
  type: Message['type'];
  expiresAt?: string;
};

export function apiToMessage(m: ApiStoredMessage): Message {
  return {
    id: m.id,
    senderId: m.senderId,
    content: m.content,
    timestamp: new Date(m.timestamp),
    status: m.status,
    type: m.type,
    expiresAt: m.expiresAt ? new Date(m.expiresAt) : undefined,
  };
}

export function messageToApi(chatId: string, m: Message): ApiStoredMessage {
  return {
    id: m.id,
    chatId,
    senderId: m.senderId,
    content: m.content,
    timestamp: m.timestamp.toISOString(),
    status: m.status,
    type: m.type,
    expiresAt: m.expiresAt ? m.expiresAt.toISOString() : undefined,
  };
}

export function mergeMessages(a: Message[], b: Message[]): Message[] {
  const byId = new Map<string, Message>();
  for (const m of a) byId.set(m.id, m);
  for (const m of b) {
    const existing = byId.get(m.id);
    if (!existing) byId.set(m.id, m);
    else {
      // Prefer the newer status if timestamps match.
      byId.set(m.id, { ...existing, ...m });
    }
  }
  return Array.from(byId.values()).sort((x, y) => x.timestamp.getTime() - y.timestamp.getTime());
}

export function filterExpired(messages: Message[]): Message[] {
  const now = Date.now();
  return messages.filter((m) => !m.expiresAt || m.expiresAt.getTime() > now);
}
