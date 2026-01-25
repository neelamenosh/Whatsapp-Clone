// Reactions Domain Service
// Handles adding, removing, and querying message reactions

import type { Reaction, AddReactionRequest } from '@/lib/contracts/features';
import type { EventBus, DomainEvent } from '@/lib/platform/events/types';
import { createEvent } from '@/lib/platform/events/types';

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface ReactionStore {
  addReaction(reaction: Reaction): Promise<Reaction>;
  removeReaction(messageId: string, userId: string, emoji: string): Promise<boolean>;
  getReactionsForMessage(messageId: string): Promise<Reaction[]>;
  getReactionsByUser(userId: string, limit?: number): Promise<Reaction[]>;
  getReactionCounts(messageId: string): Promise<Map<string, number>>;
}

// ============================================================================
// EVENTS
// ============================================================================

export const ReactionEventTypes = {
  REACTION_ADDED: 'REACTION_ADDED',
  REACTION_REMOVED: 'REACTION_REMOVED',
} as const;

export interface ReactionAddedPayload {
  messageId: string;
  chatId: string;
  userId: string;
  emoji: string;
  timestamp: string;
}

export interface ReactionRemovedPayload {
  messageId: string;
  chatId: string;
  userId: string;
  emoji: string;
}

export function reactionAddedEvent(
  payload: ReactionAddedPayload,
  correlationId?: string
): DomainEvent<ReactionAddedPayload> {
  return createEvent(ReactionEventTypes.REACTION_ADDED, payload, { correlationId });
}

export function reactionRemovedEvent(
  payload: ReactionRemovedPayload,
  correlationId?: string
): DomainEvent<ReactionRemovedPayload> {
  return createEvent(ReactionEventTypes.REACTION_REMOVED, payload, { correlationId });
}

// ============================================================================
// SERVICE
// ============================================================================

export interface AddReactionCommand {
  messageId: string;
  chatId: string;
  userId: string;
  emoji: string;
}

export interface RemoveReactionCommand {
  messageId: string;
  chatId: string;
  userId: string;
  emoji: string;
}

export async function addReaction(
  store: ReactionStore,
  command: AddReactionCommand,
  eventBus?: EventBus | null
): Promise<Reaction> {
  // Check if user already reacted with this emoji
  const existing = await store.getReactionsForMessage(command.messageId);
  const alreadyReacted = existing.find(
    r => r.userId === command.userId && r.emoji === command.emoji
  );
  
  if (alreadyReacted) {
    return alreadyReacted; // Idempotent
  }
  
  const reaction: Reaction = {
    emoji: command.emoji,
    userId: command.userId,
    messageId: command.messageId,
    timestamp: new Date().toISOString(),
  };
  
  const saved = await store.addReaction(reaction);
  
  // Publish event
  if (eventBus) {
    await eventBus.publish(reactionAddedEvent({
      messageId: command.messageId,
      chatId: command.chatId,
      userId: command.userId,
      emoji: command.emoji,
      timestamp: saved.timestamp,
    }));
  }
  
  return saved;
}

export async function removeReaction(
  store: ReactionStore,
  command: RemoveReactionCommand,
  eventBus?: EventBus | null
): Promise<boolean> {
  const removed = await store.removeReaction(
    command.messageId,
    command.userId,
    command.emoji
  );
  
  if (removed && eventBus) {
    await eventBus.publish(reactionRemovedEvent({
      messageId: command.messageId,
      chatId: command.chatId,
      userId: command.userId,
      emoji: command.emoji,
    }));
  }
  
  return removed;
}

export async function getMessageReactions(
  store: ReactionStore,
  messageId: string
): Promise<{ reactions: Reaction[]; counts: Record<string, number> }> {
  const reactions = await store.getReactionsForMessage(messageId);
  const countsMap = await store.getReactionCounts(messageId);
  
  const counts: Record<string, number> = {};
  countsMap.forEach((count, emoji) => {
    counts[emoji] = count;
  });
  
  return { reactions, counts };
}

// ============================================================================
// IN-MEMORY STORE (for development)
// ============================================================================

export class MemoryReactionStore implements ReactionStore {
  private reactions: Reaction[] = [];
  
  async addReaction(reaction: Reaction): Promise<Reaction> {
    this.reactions.push(reaction);
    return reaction;
  }
  
  async removeReaction(messageId: string, userId: string, emoji: string): Promise<boolean> {
    const index = this.reactions.findIndex(
      r => r.messageId === messageId && r.userId === userId && r.emoji === emoji
    );
    if (index >= 0) {
      this.reactions.splice(index, 1);
      return true;
    }
    return false;
  }
  
  async getReactionsForMessage(messageId: string): Promise<Reaction[]> {
    return this.reactions.filter(r => r.messageId === messageId);
  }
  
  async getReactionsByUser(userId: string, limit = 100): Promise<Reaction[]> {
    return this.reactions
      .filter(r => r.userId === userId)
      .slice(0, limit);
  }
  
  async getReactionCounts(messageId: string): Promise<Map<string, number>> {
    const counts = new Map<string, number>();
    for (const r of this.reactions) {
      if (r.messageId === messageId) {
        counts.set(r.emoji, (counts.get(r.emoji) ?? 0) + 1);
      }
    }
    return counts;
  }
}

let reactionStore: ReactionStore | null = null;

export function getReactionStore(): ReactionStore {
  if (!reactionStore) {
    reactionStore = new MemoryReactionStore();
  }
  return reactionStore;
}

// Factory function for ReactionService
let reactionServiceInstance: ReactionService | null = null;

export function getReactionService(): ReactionService {
  if (!reactionServiceInstance) {
    const eventBus: EventBus = {
      publish: async () => {},
      subscribe: () => () => {},
    };
    reactionServiceInstance = new ReactionService(getReactionStore(), eventBus);
  }
  return reactionServiceInstance;
}
