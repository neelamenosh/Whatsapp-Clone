// Read Receipts Service
// Handles delivery and read status tracking

import type { ReadReceipt, ReadReceiptBatch } from '@/lib/contracts/features';
import type { EventBus, DomainEvent } from '@/lib/platform/events/types';
import { createEvent } from '@/lib/platform/events/types';

// ============================================================================
// EVENTS
// ============================================================================

export const ReceiptEventTypes = {
  MESSAGE_DELIVERED: 'MESSAGE_DELIVERED',
  MESSAGE_READ: 'MESSAGE_READ',
  MESSAGES_READ_BATCH: 'MESSAGES_READ_BATCH',
} as const;

export interface MessageDeliveredPayload {
  messageId: string;
  chatId: string;
  userId: string;
  timestamp: string;
}

export interface MessageReadPayload {
  messageId: string;
  chatId: string;
  userId: string;
  timestamp: string;
}

export interface MessagesReadBatchPayload {
  chatId: string;
  userId: string;
  messageIds: string[];
  timestamp: string;
}

export function messageDeliveredEvent(
  payload: MessageDeliveredPayload
): DomainEvent<MessageDeliveredPayload> {
  return createEvent(ReceiptEventTypes.MESSAGE_DELIVERED, payload);
}

export function messageReadEvent(
  payload: MessageReadPayload
): DomainEvent<MessageReadPayload> {
  return createEvent(ReceiptEventTypes.MESSAGE_READ, payload);
}

export function messagesReadBatchEvent(
  payload: MessagesReadBatchPayload
): DomainEvent<MessagesReadBatchPayload> {
  return createEvent(ReceiptEventTypes.MESSAGES_READ_BATCH, payload);
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface ReadReceiptStore {
  setReceipt(receipt: ReadReceipt): Promise<void>;
  setReceiptsBatch(chatId: string, messageIds: string[], userId: string, status: 'delivered' | 'read'): Promise<void>;
  getReceipt(messageId: string, userId: string): Promise<ReadReceipt | null>;
  getReceiptsForMessage(messageId: string): Promise<ReadReceipt[]>;
  getUnreadCount(chatId: string, userId: string): Promise<number>;
  getLastReadMessageId(chatId: string, userId: string): Promise<string | null>;
}

// ============================================================================
// SERVICE
// ============================================================================

export interface MarkDeliveredCommand {
  messageId: string;
  chatId: string;
  userId: string;
}

export interface MarkReadCommand {
  messageId: string;
  chatId: string;
  userId: string;
}

export interface MarkReadBatchCommand {
  chatId: string;
  userId: string;
  messageIds: string[];
}

export async function markDelivered(
  store: ReadReceiptStore,
  command: MarkDeliveredCommand,
  eventBus?: EventBus | null
): Promise<ReadReceipt> {
  const timestamp = new Date().toISOString();
  
  const receipt: ReadReceipt = {
    messageId: command.messageId,
    chatId: command.chatId,
    userId: command.userId,
    status: 'delivered',
    timestamp,
  };
  
  await store.setReceipt(receipt);
  
  if (eventBus) {
    await eventBus.publish(messageDeliveredEvent({
      messageId: command.messageId,
      chatId: command.chatId,
      userId: command.userId,
      timestamp,
    }));
  }
  
  return receipt;
}

export async function markRead(
  store: ReadReceiptStore,
  command: MarkReadCommand,
  eventBus?: EventBus | null
): Promise<ReadReceipt> {
  const timestamp = new Date().toISOString();
  
  const receipt: ReadReceipt = {
    messageId: command.messageId,
    chatId: command.chatId,
    userId: command.userId,
    status: 'read',
    timestamp,
  };
  
  await store.setReceipt(receipt);
  
  if (eventBus) {
    await eventBus.publish(messageReadEvent({
      messageId: command.messageId,
      chatId: command.chatId,
      userId: command.userId,
      timestamp,
    }));
  }
  
  return receipt;
}

export async function markReadBatch(
  store: ReadReceiptStore,
  command: MarkReadBatchCommand,
  eventBus?: EventBus | null
): Promise<void> {
  const timestamp = new Date().toISOString();
  
  await store.setReceiptsBatch(
    command.chatId,
    command.messageIds,
    command.userId,
    'read'
  );
  
  if (eventBus) {
    await eventBus.publish(messagesReadBatchEvent({
      chatId: command.chatId,
      userId: command.userId,
      messageIds: command.messageIds,
      timestamp,
    }));
  }
}

export async function getMessageReceipts(
  store: ReadReceiptStore,
  messageId: string
): Promise<{
  delivered: string[];
  read: string[];
}> {
  const receipts = await store.getReceiptsForMessage(messageId);
  
  const delivered: string[] = [];
  const read: string[] = [];
  
  for (const receipt of receipts) {
    if (receipt.status === 'read') {
      read.push(receipt.userId);
    } else {
      delivered.push(receipt.userId);
    }
  }
  
  return { delivered, read };
}

export async function getUnreadCount(
  store: ReadReceiptStore,
  chatId: string,
  userId: string
): Promise<number> {
  return store.getUnreadCount(chatId, userId);
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryReadReceiptStore implements ReadReceiptStore {
  private receipts: ReadReceipt[] = [];
  
  async setReceipt(receipt: ReadReceipt): Promise<void> {
    // Update existing or add new
    const index = this.receipts.findIndex(
      r => r.messageId === receipt.messageId && r.userId === receipt.userId
    );
    
    if (index >= 0) {
      this.receipts[index] = receipt;
    } else {
      this.receipts.push(receipt);
    }
  }
  
  async setReceiptsBatch(
    chatId: string,
    messageIds: string[],
    userId: string,
    status: 'delivered' | 'read'
  ): Promise<void> {
    const timestamp = new Date().toISOString();
    
    for (const messageId of messageIds) {
      await this.setReceipt({
        messageId,
        chatId,
        userId,
        status,
        timestamp,
      });
    }
  }
  
  async getReceipt(messageId: string, userId: string): Promise<ReadReceipt | null> {
    return this.receipts.find(
      r => r.messageId === messageId && r.userId === userId
    ) ?? null;
  }
  
  async getReceiptsForMessage(messageId: string): Promise<ReadReceipt[]> {
    return this.receipts.filter(r => r.messageId === messageId);
  }
  
  async getUnreadCount(chatId: string, userId: string): Promise<number> {
    // This is simplified - real implementation would query message store
    const readReceipts = this.receipts.filter(
      r => r.chatId === chatId && r.userId === userId && r.status === 'read'
    );
    // Return 0 for demo - real implementation would count unread messages
    return 0;
  }
  
  async getLastReadMessageId(chatId: string, userId: string): Promise<string | null> {
    const receipts = this.receipts
      .filter(r => r.chatId === chatId && r.userId === userId && r.status === 'read')
      .sort((a, b) => b.timestamp.localeCompare(a.timestamp));
    
    return receipts[0]?.messageId ?? null;
  }
}

let readReceiptStore: ReadReceiptStore | null = null;

export function getReadReceiptStore(): ReadReceiptStore {
  if (!readReceiptStore) {
    readReceiptStore = new MemoryReadReceiptStore();
  }
  return readReceiptStore;
}

// Factory function for ReadReceiptService
let readReceiptServiceInstance: ReadReceiptService | null = null;

export function getReadReceiptService(): ReadReceiptService {
  if (!readReceiptServiceInstance) {
    const eventBus: EventBus = {
      publish: async () => {},
      subscribe: () => () => {},
    };
    readReceiptServiceInstance = new ReadReceiptService(getReadReceiptStore(), eventBus);
  }
  return readReceiptServiceInstance;
}
