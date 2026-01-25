// Store configuration - switch implementations via environment variables.
// In production, swap these for Postgres/Redis implementations.

import type { MessageStore } from '@/lib/domain/messaging/store';
import type { IdempotencyStore } from '@/lib/domain/messaging/idempotency';

// Default to 'memory' for serverless environments (Vercel)
// Use 'fs' for local development with file-based persistence
const STORE_TYPE = process.env.STORE_TYPE ?? 'memory'; // 'memory' | 'fs' | 'postgres'

let messageStore: MessageStore;
let idempotencyStore: IdempotencyStore;

if (STORE_TYPE === 'fs') {
  // File-based JSON - for local development with persistence
  const { fsMessageStore } = require('./message-store/fs-message-store');
  const { memoryIdempotencyStore } = require('./idempotency/memory-idempotency-store');
  messageStore = fsMessageStore;
  idempotencyStore = memoryIdempotencyStore;
} else {
  // Memory store - works on serverless (Vercel)
  const { memoryIdempotencyStore } = require('./idempotency/memory-idempotency-store');
  // Create a simple in-memory message store
  const messages = new Map<string, any[]>();
  messageStore = {
    async appendMessage(chatId: string, message: any) {
      if (!messages.has(chatId)) messages.set(chatId, []);
      messages.get(chatId)!.push(message);
      return message;
    },
    async getMessages(chatId: string, options?: any) {
      return messages.get(chatId) ?? [];
    },
    async getMessage(chatId: string, messageId: string) {
      const chatMessages = messages.get(chatId) ?? [];
      return chatMessages.find((m: any) => m.id === messageId) ?? null;
    },
    async updateMessageStatus(chatId: string, messageId: string, status: string) {
      const chatMessages = messages.get(chatId) ?? [];
      const msg = chatMessages.find((m: any) => m.id === messageId);
      if (msg) msg.status = status;
      return msg ?? null;
    },
    async deleteMessage(chatId: string, messageId: string) {
      const chatMessages = messages.get(chatId) ?? [];
      const index = chatMessages.findIndex((m: any) => m.id === messageId);
      if (index >= 0) {
        chatMessages.splice(index, 1);
        return true;
      }
      return false;
    },
  };
  idempotencyStore = memoryIdempotencyStore;
}

export { messageStore, idempotencyStore };
