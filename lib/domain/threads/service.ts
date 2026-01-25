// Threaded Conversations Service
// Handles message threads, replies, and thread management

import type { Thread, QuotedMessage } from '@/lib/contracts/features';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const THREAD_EVENTS = {
  THREAD_CREATED: 'thread:created',
  THREAD_REPLY_ADDED: 'thread:reply_added',
  THREAD_PARTICIPANTS_UPDATED: 'thread:participants_updated',
} as const;

export interface ThreadCreatedEvent {
  type: typeof THREAD_EVENTS.THREAD_CREATED;
  payload: Thread;
}

export interface ThreadReplyAddedEvent {
  type: typeof THREAD_EVENTS.THREAD_REPLY_ADDED;
  payload: {
    threadId: string;
    messageId: string;
    replyCount: number;
    lastReplyAt: string;
  };
}

export interface ThreadParticipantsUpdatedEvent {
  type: typeof THREAD_EVENTS.THREAD_PARTICIPANTS_UPDATED;
  payload: {
    threadId: string;
    participants: string[];
  };
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface ThreadStore {
  createThread(thread: Omit<Thread, 'id'>): Promise<Thread>;
  getThread(threadId: string): Promise<Thread | null>;
  getThreadByParentMessage(messageId: string): Promise<Thread | null>;
  updateThread(threadId: string, updates: Partial<Thread>): Promise<Thread | null>;
  addReply(threadId: string, messageId: string): Promise<Thread | null>;
  addParticipant(threadId: string, userId: string): Promise<Thread | null>;
  getThreadMessages(threadId: string): Promise<string[]>;
  
  // Quoted messages
  createQuotedMessage(quote: Omit<QuotedMessage, 'id'>): Promise<QuotedMessage>;
  getQuotedMessage(messageId: string): Promise<QuotedMessage | null>;
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryThreadStore implements ThreadStore {
  private threads = new Map<string, Thread>();
  private threadsByMessage = new Map<string, string>(); // messageId -> threadId
  private quotedMessages = new Map<string, QuotedMessage>(); // quoting messageId -> QuotedMessage
  
  async createThread(data: Omit<Thread, 'id'>): Promise<Thread> {
    const id = `thread-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const thread: Thread = {
      id,
      ...data,
    };
    
    this.threads.set(id, thread);
    this.threadsByMessage.set(data.parentMessageId, id);
    
    return thread;
  }
  
  async getThread(threadId: string): Promise<Thread | null> {
    return this.threads.get(threadId) ?? null;
  }
  
  async getThreadByParentMessage(messageId: string): Promise<Thread | null> {
    const threadId = this.threadsByMessage.get(messageId);
    if (!threadId) return null;
    return this.threads.get(threadId) ?? null;
  }
  
  async updateThread(threadId: string, updates: Partial<Thread>): Promise<Thread | null> {
    const existing = this.threads.get(threadId);
    if (!existing) return null;
    
    const updated: Thread = { ...existing, ...updates, id: threadId };
    this.threads.set(threadId, updated);
    return updated;
  }
  
  async addReply(threadId: string, messageId: string): Promise<Thread | null> {
    const existing = this.threads.get(threadId);
    if (!existing) return null;
    
    const updated: Thread = {
      ...existing,
      replyCount: existing.replyCount + 1,
      lastReplyAt: new Date().toISOString(),
    };
    
    this.threads.set(threadId, updated);
    return updated;
  }
  
  async addParticipant(threadId: string, userId: string): Promise<Thread | null> {
    const existing = this.threads.get(threadId);
    if (!existing) return null;
    
    if (existing.participants.includes(userId)) {
      return existing;
    }
    
    const updated: Thread = {
      ...existing,
      participants: [...existing.participants, userId],
    };
    
    this.threads.set(threadId, updated);
    return updated;
  }
  
  async getThreadMessages(threadId: string): Promise<string[]> {
    const thread = this.threads.get(threadId);
    return thread ? [thread.parentMessageId] : [];
  }
  
  async createQuotedMessage(data: Omit<QuotedMessage, 'id'>): Promise<QuotedMessage> {
    const quote: QuotedMessage = { ...data } as QuotedMessage;
    this.quotedMessages.set(data.quotingMessageId, quote);
    return quote;
  }
  
  async getQuotedMessage(messageId: string): Promise<QuotedMessage | null> {
    return this.quotedMessages.get(messageId) ?? null;
  }
}

// ============================================================================
// SERVICE
// ============================================================================

export interface ThreadService {
  createThread(parentMessageId: string, chatId: string, creatorId: string): Promise<Thread>;
  getOrCreateThread(parentMessageId: string, chatId: string, creatorId: string): Promise<Thread>;
  addReplyToThread(threadId: string, messageId: string, userId: string): Promise<Thread | null>;
  getThread(threadId: string): Promise<Thread | null>;
  getThreadByParent(messageId: string): Promise<Thread | null>;
  
  // Quoted messages
  createQuote(
    quotingMessageId: string,
    originalMessageId: string,
    originalSenderId: string,
    originalContent: string,
    originalTimestamp: string
  ): Promise<QuotedMessage>;
  getQuote(messageId: string): Promise<QuotedMessage | null>;
}

export class ThreadServiceImpl implements ThreadService {
  private store: ThreadStore;
  private eventBus: EventBus;
  
  constructor(store: ThreadStore, eventBus: EventBus) {
    this.store = store;
    this.eventBus = eventBus;
  }
  
  async createThread(parentMessageId: string, chatId: string, creatorId: string): Promise<Thread> {
    const thread = await this.store.createThread({
      parentMessageId,
      chatId,
      replyCount: 0,
      participants: [creatorId],
      createdAt: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: THREAD_EVENTS.THREAD_CREATED,
      payload: thread,
    });
    
    return thread;
  }
  
  async getOrCreateThread(parentMessageId: string, chatId: string, creatorId: string): Promise<Thread> {
    const existing = await this.store.getThreadByParentMessage(parentMessageId);
    if (existing) {
      // Add participant if not already in thread
      await this.store.addParticipant(existing.id, creatorId);
      return existing;
    }
    
    return this.createThread(parentMessageId, chatId, creatorId);
  }
  
  async addReplyToThread(threadId: string, messageId: string, userId: string): Promise<Thread | null> {
    // Add the reply
    const updated = await this.store.addReply(threadId, messageId);
    if (!updated) return null;
    
    // Add user as participant
    await this.store.addParticipant(threadId, userId);
    
    this.eventBus.publish({
      type: THREAD_EVENTS.THREAD_REPLY_ADDED,
      payload: {
        threadId,
        messageId,
        replyCount: updated.replyCount,
        lastReplyAt: updated.lastReplyAt!,
      },
    });
    
    return updated;
  }
  
  async getThread(threadId: string): Promise<Thread | null> {
    return this.store.getThread(threadId);
  }
  
  async getThreadByParent(messageId: string): Promise<Thread | null> {
    return this.store.getThreadByParentMessage(messageId);
  }
  
  async createQuote(
    quotingMessageId: string,
    originalMessageId: string,
    originalSenderId: string,
    originalContent: string,
    originalTimestamp: string
  ): Promise<QuotedMessage> {
    return this.store.createQuotedMessage({
      quotingMessageId,
      originalMessageId,
      originalSenderId,
      originalContent: originalContent.slice(0, 500), // Truncate for preview
      originalTimestamp,
      originalType: 'text',
    });
  }
  
  async getQuote(messageId: string): Promise<QuotedMessage | null> {
    return this.store.getQuotedMessage(messageId);
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let threadService: ThreadService | null = null;

export function getThreadService(): ThreadService {
  if (!threadService) {
    threadService = new ThreadServiceImpl(new MemoryThreadStore(), getEventBus());
  }
  return threadService;
}
