// Poll Service
// Handles creation, voting, and management of polls

import type { Poll, PollOption, CreatePollRequest, VotePollRequest } from '@/lib/contracts/engagement';
import type { EventBus, DomainEvent } from '@/lib/platform/events/types';
import { createEvent } from '@/lib/platform/events/types';

// ============================================================================
// EVENTS
// ============================================================================

export const PollEventTypes = {
  POLL_CREATED: 'POLL_CREATED',
  POLL_VOTED: 'POLL_VOTED',
  POLL_CLOSED: 'POLL_CLOSED',
  POLL_OPTION_ADDED: 'POLL_OPTION_ADDED',
} as const;

export interface PollCreatedPayload {
  poll: Poll;
}

export interface PollVotedPayload {
  pollId: string;
  chatId: string;
  userId: string;
  optionIds: string[];
  timestamp: string;
}

export interface PollClosedPayload {
  pollId: string;
  chatId: string;
  closedBy: string;
  finalResults: { optionId: string; voteCount: number }[];
}

export function pollCreatedEvent(payload: PollCreatedPayload): DomainEvent<PollCreatedPayload> {
  return createEvent(PollEventTypes.POLL_CREATED, payload);
}

export function pollVotedEvent(payload: PollVotedPayload): DomainEvent<PollVotedPayload> {
  return createEvent(PollEventTypes.POLL_VOTED, payload);
}

export function pollClosedEvent(payload: PollClosedPayload): DomainEvent<PollClosedPayload> {
  return createEvent(PollEventTypes.POLL_CLOSED, payload);
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface PollStore {
  createPoll(poll: Poll): Promise<Poll>;
  getPoll(pollId: string): Promise<Poll | null>;
  getPollsForChat(chatId: string): Promise<Poll[]>;
  vote(pollId: string, userId: string, optionIds: string[]): Promise<Poll>;
  removeVote(pollId: string, userId: string): Promise<Poll>;
  closePoll(pollId: string): Promise<Poll>;
  addOption(pollId: string, option: PollOption): Promise<Poll>;
}

// ============================================================================
// SERVICE
// ============================================================================

export interface CreatePollCommand {
  chatId: string;
  creatorId: string;
  question: string;
  options: string[];
  allowMultipleVotes?: boolean;
  allowAddOptions?: boolean;
  isAnonymous?: boolean;
  closesAt?: string;
}

export interface VotePollCommand {
  pollId: string;
  userId: string;
  optionIds: string[];
}

export async function createPoll(
  store: PollStore,
  command: CreatePollCommand,
  eventBus?: EventBus | null
): Promise<Poll> {
  const pollId = `poll-${Date.now()}-${Math.random().toString(36).slice(2)}`;
  const messageId = `msg-${pollId}`;
  const timestamp = new Date().toISOString();
  
  const options: PollOption[] = command.options.map((text, index) => ({
    id: `opt-${pollId}-${index}`,
    text,
    voterIds: [],
    voteCount: 0,
  }));
  
  const poll: Poll = {
    id: pollId,
    messageId,
    chatId: command.chatId,
    creatorId: command.creatorId,
    question: command.question,
    options,
    allowMultipleVotes: command.allowMultipleVotes ?? false,
    allowAddOptions: command.allowAddOptions ?? false,
    isAnonymous: command.isAnonymous ?? false,
    showResultsBeforeVote: true,
    totalVotes: 0,
    status: 'active',
    createdAt: timestamp,
    closesAt: command.closesAt,
  };
  
  const saved = await store.createPoll(poll);
  
  if (eventBus) {
    await eventBus.publish(pollCreatedEvent({ poll: saved }));
  }
  
  return saved;
}

export async function votePoll(
  store: PollStore,
  command: VotePollCommand,
  eventBus?: EventBus | null
): Promise<Poll> {
  const poll = await store.getPoll(command.pollId);
  
  if (!poll) {
    const error = new Error('Poll not found');
    (error as any).statusCode = 404;
    throw error;
  }
  
  if (poll.status !== 'active') {
    const error = new Error('Poll is closed');
    (error as any).statusCode = 400;
    throw error;
  }
  
  if (!poll.allowMultipleVotes && command.optionIds.length > 1) {
    const error = new Error('Multiple votes not allowed');
    (error as any).statusCode = 400;
    throw error;
  }
  
  // Validate option IDs
  const validOptionIds = new Set(poll.options.map(o => o.id));
  for (const optionId of command.optionIds) {
    if (!validOptionIds.has(optionId)) {
      const error = new Error(`Invalid option ID: ${optionId}`);
      (error as any).statusCode = 400;
      throw error;
    }
  }
  
  const updated = await store.vote(command.pollId, command.userId, command.optionIds);
  
  if (eventBus) {
    await eventBus.publish(pollVotedEvent({
      pollId: command.pollId,
      chatId: poll.chatId,
      userId: command.userId,
      optionIds: command.optionIds,
      timestamp: new Date().toISOString(),
    }));
  }
  
  return updated;
}

export async function closePoll(
  store: PollStore,
  pollId: string,
  closedBy: string,
  eventBus?: EventBus | null
): Promise<Poll> {
  const poll = await store.getPoll(pollId);
  
  if (!poll) {
    const error = new Error('Poll not found');
    (error as any).statusCode = 404;
    throw error;
  }
  
  const closed = await store.closePoll(pollId);
  
  if (eventBus) {
    await eventBus.publish(pollClosedEvent({
      pollId,
      chatId: poll.chatId,
      closedBy,
      finalResults: closed.options.map(o => ({
        optionId: o.id,
        voteCount: o.voteCount,
      })),
    }));
  }
  
  return closed;
}

export async function getPoll(store: PollStore, pollId: string): Promise<Poll | null> {
  return store.getPoll(pollId);
}

export async function getPollsForChat(store: PollStore, chatId: string): Promise<Poll[]> {
  return store.getPollsForChat(chatId);
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryPollStore implements PollStore {
  private polls = new Map<string, Poll>();
  
  async createPoll(poll: Poll): Promise<Poll> {
    this.polls.set(poll.id, poll);
    return poll;
  }
  
  async getPoll(pollId: string): Promise<Poll | null> {
    return this.polls.get(pollId) ?? null;
  }
  
  async getPollsForChat(chatId: string): Promise<Poll[]> {
    return Array.from(this.polls.values()).filter(p => p.chatId === chatId);
  }
  
  async vote(pollId: string, userId: string, optionIds: string[]): Promise<Poll> {
    const poll = this.polls.get(pollId);
    if (!poll) throw new Error('Poll not found');
    
    // Remove previous votes
    for (const option of poll.options) {
      const index = option.voterIds.indexOf(userId);
      if (index >= 0) {
        option.voterIds.splice(index, 1);
        option.voteCount--;
        poll.totalVotes--;
      }
    }
    
    // Add new votes
    for (const optionId of optionIds) {
      const option = poll.options.find(o => o.id === optionId);
      if (option) {
        option.voterIds.push(userId);
        option.voteCount++;
        poll.totalVotes++;
      }
    }
    
    return poll;
  }
  
  async removeVote(pollId: string, userId: string): Promise<Poll> {
    const poll = this.polls.get(pollId);
    if (!poll) throw new Error('Poll not found');
    
    for (const option of poll.options) {
      const index = option.voterIds.indexOf(userId);
      if (index >= 0) {
        option.voterIds.splice(index, 1);
        option.voteCount--;
        poll.totalVotes--;
      }
    }
    
    return poll;
  }
  
  async closePoll(pollId: string): Promise<Poll> {
    const poll = this.polls.get(pollId);
    if (!poll) throw new Error('Poll not found');
    
    poll.status = 'closed';
    poll.closedAt = new Date().toISOString();
    
    return poll;
  }
  
  async addOption(pollId: string, option: PollOption): Promise<Poll> {
    const poll = this.polls.get(pollId);
    if (!poll) throw new Error('Poll not found');
    
    poll.options.push(option);
    return poll;
  }
}

let pollStore: PollStore | null = null;

export function getPollStore(): PollStore {
  if (!pollStore) {
    pollStore = new MemoryPollStore();
  }
  return pollStore;
}
