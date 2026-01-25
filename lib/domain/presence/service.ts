// Typing Indicator and User Presence Service

import type { UserPresence, TypingIndicator, PresenceStatus } from '@/lib/contracts/features';
import type { EventBus, DomainEvent } from '@/lib/platform/events/types';
import { createEvent } from '@/lib/platform/events/types';

// ============================================================================
// EVENTS
// ============================================================================

export const PresenceEventTypes = {
  PRESENCE_CHANGED: 'PRESENCE_CHANGED',
  TYPING_STARTED: 'TYPING_STARTED',
  TYPING_STOPPED: 'TYPING_STOPPED',
} as const;

export interface PresenceChangedPayload {
  userId: string;
  status: PresenceStatus;
  lastSeen?: string;
  customStatus?: string;
}

export interface TypingPayload {
  chatId: string;
  userId: string;
}

export function presenceChangedEvent(
  payload: PresenceChangedPayload
): DomainEvent<PresenceChangedPayload> {
  return createEvent(PresenceEventTypes.PRESENCE_CHANGED, payload, { source: 'presence-service' });
}

export function typingStartedEvent(
  payload: TypingPayload
): DomainEvent<TypingPayload> {
  return createEvent(PresenceEventTypes.TYPING_STARTED, payload, { source: 'presence-service' });
}

export function typingStoppedEvent(
  payload: TypingPayload
): DomainEvent<TypingPayload> {
  return createEvent(PresenceEventTypes.TYPING_STOPPED, payload, { source: 'presence-service' });
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface PresenceStore {
  setPresence(presence: UserPresence): Promise<void>;
  getPresence(userId: string): Promise<UserPresence | null>;
  getPresences(userIds: string[]): Promise<Map<string, UserPresence>>;
  setTyping(chatId: string, userId: string, isTyping: boolean): Promise<void>;
  getTypingUsers(chatId: string): Promise<string[]>;
}

// ============================================================================
// SERVICE
// ============================================================================

const TYPING_TIMEOUT_MS = 5000; // Auto-clear typing after 5 seconds

export interface UpdatePresenceCommand {
  userId: string;
  status: PresenceStatus;
  customStatus?: string;
}

export async function updatePresence(
  store: PresenceStore,
  command: UpdatePresenceCommand,
  eventBus?: EventBus | null
): Promise<UserPresence> {
  const lastSeen = command.status === 'offline' 
    ? new Date().toISOString() 
    : undefined;
  
  const presence: UserPresence = {
    userId: command.userId,
    status: command.status,
    lastSeen,
    customStatus: command.customStatus,
  };
  
  await store.setPresence(presence);
  
  if (eventBus) {
    await eventBus.publish(presenceChangedEvent({
      userId: command.userId,
      status: command.status,
      lastSeen,
      customStatus: command.customStatus,
    }));
  }
  
  return presence;
}

export async function startTyping(
  store: PresenceStore,
  chatId: string,
  userId: string,
  eventBus?: EventBus | null
): Promise<void> {
  await store.setTyping(chatId, userId, true);
  
  if (eventBus) {
    await eventBus.publish(typingStartedEvent({ chatId, userId }));
  }
}

export async function stopTyping(
  store: PresenceStore,
  chatId: string,
  userId: string,
  eventBus?: EventBus | null
): Promise<void> {
  await store.setTyping(chatId, userId, false);
  
  if (eventBus) {
    await eventBus.publish(typingStoppedEvent({ chatId, userId }));
  }
}

export async function getPresence(
  store: PresenceStore,
  userId: string
): Promise<UserPresence | null> {
  return store.getPresence(userId);
}

export async function getBulkPresence(
  store: PresenceStore,
  userIds: string[]
): Promise<Map<string, UserPresence>> {
  return store.getPresences(userIds);
}

export async function getTypingUsers(
  store: PresenceStore,
  chatId: string
): Promise<string[]> {
  return store.getTypingUsers(chatId);
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryPresenceStore implements PresenceStore {
  private presences = new Map<string, UserPresence>();
  private typing = new Map<string, Set<string>>(); // chatId -> userIds
  private typingTimers = new Map<string, NodeJS.Timeout>(); // `${chatId}:${userId}` -> timer
  
  async setPresence(presence: UserPresence): Promise<void> {
    this.presences.set(presence.userId, presence);
  }
  
  async getPresence(userId: string): Promise<UserPresence | null> {
    return this.presences.get(userId) ?? null;
  }
  
  async getPresences(userIds: string[]): Promise<Map<string, UserPresence>> {
    const result = new Map<string, UserPresence>();
    for (const userId of userIds) {
      const presence = this.presences.get(userId);
      if (presence) {
        result.set(userId, presence);
      }
    }
    return result;
  }
  
  async setTyping(chatId: string, userId: string, isTyping: boolean): Promise<void> {
    const key = `${chatId}:${userId}`;
    
    // Clear existing timer
    const existingTimer = this.typingTimers.get(key);
    if (existingTimer) {
      clearTimeout(existingTimer);
      this.typingTimers.delete(key);
    }
    
    if (!this.typing.has(chatId)) {
      this.typing.set(chatId, new Set());
    }
    
    const chatTyping = this.typing.get(chatId)!;
    
    if (isTyping) {
      chatTyping.add(userId);
      
      // Auto-clear after timeout
      const timer = setTimeout(() => {
        chatTyping.delete(userId);
        this.typingTimers.delete(key);
      }, TYPING_TIMEOUT_MS);
      
      this.typingTimers.set(key, timer);
    } else {
      chatTyping.delete(userId);
    }
  }
  
  async getTypingUsers(chatId: string): Promise<string[]> {
    return Array.from(this.typing.get(chatId) ?? []);
  }
}

let presenceStore: PresenceStore | null = null;

export function getPresenceStore(): PresenceStore {
  if (!presenceStore) {
    presenceStore = new MemoryPresenceStore();
  }
  return presenceStore;
}
