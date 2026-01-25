// Advanced Search Service
// Handles message and conversation search with filters

import type {
  SearchFilter,
  AdvancedSearchRequest,
  AdvancedSearchResponse,
  SearchResultItem,
  ConversationSearchRequest,
  ConversationSearchResponse,
  SearchHistory,
  SearchSuggestion,
} from '@/lib/contracts/search';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const SEARCH_EVENTS = {
  SEARCH_PERFORMED: 'search:performed',
  SEARCH_RESULT_CLICKED: 'search:result_clicked',
} as const;

// ============================================================================
// SEARCH INDEX INTERFACE
// ============================================================================

export interface SearchIndex {
  indexMessage(message: IndexableMessage): Promise<void>;
  removeMessage(messageId: string): Promise<void>;
  search(query: string, options?: SearchOptions): Promise<SearchResult[]>;
  searchInChat(chatId: string, query: string, options?: SearchOptions): Promise<SearchResult[]>;
  getSuggestions(prefix: string, userId: string): Promise<string[]>;
}

export interface IndexableMessage {
  id: string;
  chatId: string;
  senderId: string;
  senderName?: string;
  content: string;
  type: string;
  timestamp: string;
  metadata?: Record<string, unknown>;
}

export interface SearchOptions {
  limit?: number;
  offset?: number;
  dateRange?: { from?: string; to?: string };
  messageTypes?: string[];
  senderIds?: string[];
  chatIds?: string[];
  hasMedia?: boolean;
  hasLinks?: boolean;
}

export interface SearchResult {
  messageId: string;
  chatId: string;
  senderId: string;
  senderName?: string;
  content: string;
  snippet: string;
  type: string;
  timestamp: string;
  relevanceScore: number;
  highlights: string[];
}

// ============================================================================
// IN-MEMORY SEARCH INDEX
// ============================================================================

export class MemorySearchIndex implements SearchIndex {
  private messages = new Map<string, IndexableMessage>();
  private searchHistory = new Map<string, string[]>(); // userId -> queries
  
  async indexMessage(message: IndexableMessage): Promise<void> {
    this.messages.set(message.id, message);
  }
  
  async removeMessage(messageId: string): Promise<void> {
    this.messages.delete(messageId);
  }
  
  async search(query: string, options?: SearchOptions): Promise<SearchResult[]> {
    const results: SearchResult[] = [];
    const lowerQuery = query.toLowerCase();
    const limit = options?.limit ?? 50;
    const offset = options?.offset ?? 0;
    
    for (const message of this.messages.values()) {
      // Apply filters
      if (options?.chatIds?.length && !options.chatIds.includes(message.chatId)) continue;
      if (options?.senderIds?.length && !options.senderIds.includes(message.senderId)) continue;
      if (options?.messageTypes?.length && !options.messageTypes.includes(message.type)) continue;
      
      if (options?.dateRange?.from) {
        if (new Date(message.timestamp) < new Date(options.dateRange.from)) continue;
      }
      if (options?.dateRange?.to) {
        if (new Date(message.timestamp) > new Date(options.dateRange.to)) continue;
      }
      
      // Check content match
      const lowerContent = message.content.toLowerCase();
      if (!lowerContent.includes(lowerQuery)) continue;
      
      // Calculate relevance
      let relevanceScore = 0;
      const matchIndex = lowerContent.indexOf(lowerQuery);
      
      // Exact match boost
      if (lowerContent === lowerQuery) relevanceScore += 10;
      // Start match boost
      else if (matchIndex === 0) relevanceScore += 5;
      // Word boundary match
      else if (matchIndex > 0 && /\s/.test(lowerContent[matchIndex - 1])) relevanceScore += 3;
      // Basic match
      else relevanceScore += 1;
      
      // Recency boost
      const age = Date.now() - new Date(message.timestamp).getTime();
      const dayMs = 24 * 60 * 60 * 1000;
      if (age < dayMs) relevanceScore += 2;
      else if (age < 7 * dayMs) relevanceScore += 1;
      
      // Create snippet with highlighted match
      const snippetStart = Math.max(0, matchIndex - 30);
      const snippetEnd = Math.min(message.content.length, matchIndex + query.length + 30);
      let snippet = message.content.slice(snippetStart, snippetEnd);
      if (snippetStart > 0) snippet = '...' + snippet;
      if (snippetEnd < message.content.length) snippet = snippet + '...';
      
      results.push({
        messageId: message.id,
        chatId: message.chatId,
        senderId: message.senderId,
        senderName: message.senderName,
        content: message.content,
        snippet,
        type: message.type,
        timestamp: message.timestamp,
        relevanceScore,
        highlights: [query],
      });
    }
    
    // Sort by relevance and recency
    results.sort((a, b) => {
      if (b.relevanceScore !== a.relevanceScore) {
        return b.relevanceScore - a.relevanceScore;
      }
      return new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime();
    });
    
    return results.slice(offset, offset + limit);
  }
  
  async searchInChat(chatId: string, query: string, options?: SearchOptions): Promise<SearchResult[]> {
    return this.search(query, { ...options, chatIds: [chatId] });
  }
  
  async getSuggestions(prefix: string, userId: string): Promise<string[]> {
    const history = this.searchHistory.get(userId) ?? [];
    const lowerPrefix = prefix.toLowerCase();
    
    return history
      .filter(q => q.toLowerCase().startsWith(lowerPrefix))
      .slice(0, 5);
  }
  
  recordSearch(userId: string, query: string): void {
    if (!this.searchHistory.has(userId)) {
      this.searchHistory.set(userId, []);
    }
    const history = this.searchHistory.get(userId)!;
    // Remove if exists
    const idx = history.indexOf(query);
    if (idx > -1) history.splice(idx, 1);
    // Add to front
    history.unshift(query);
    // Keep max 50
    if (history.length > 50) history.pop();
  }
}

// ============================================================================
// SEARCH HISTORY STORE
// ============================================================================

export interface SearchHistoryStore {
  addSearch(userId: string, query: string): Promise<SearchHistory>;
  getHistory(userId: string, limit?: number): Promise<SearchHistory[]>;
  clearHistory(userId: string): Promise<void>;
  deleteSearch(userId: string, searchId: string): Promise<void>;
}

export class MemorySearchHistoryStore implements SearchHistoryStore {
  private history = new Map<string, SearchHistory[]>();
  
  async addSearch(userId: string, query: string): Promise<SearchHistory> {
    if (!this.history.has(userId)) {
      this.history.set(userId, []);
    }
    
    const entry: SearchHistory = {
      id: `sh-${Date.now()}-${Math.random().toString(36).slice(2)}`,
      userId,
      query,
      timestamp: new Date().toISOString(),
      resultCount: 0,
    };
    
    this.history.get(userId)!.unshift(entry);
    
    // Keep max 100 entries
    const userHistory = this.history.get(userId)!;
    if (userHistory.length > 100) {
      userHistory.pop();
    }
    
    return entry;
  }
  
  async getHistory(userId: string, limit: number = 20): Promise<SearchHistory[]> {
    return (this.history.get(userId) ?? []).slice(0, limit);
  }
  
  async clearHistory(userId: string): Promise<void> {
    this.history.delete(userId);
  }
  
  async deleteSearch(userId: string, searchId: string): Promise<void> {
    const userHistory = this.history.get(userId);
    if (!userHistory) return;
    
    const idx = userHistory.findIndex(h => h.id === searchId);
    if (idx > -1) userHistory.splice(idx, 1);
  }
}

// ============================================================================
// SEARCH SERVICE
// ============================================================================

export interface AdvancedSearchService {
  search(request: AdvancedSearchRequest): Promise<AdvancedSearchResponse>;
  searchInConversation(request: ConversationSearchRequest): Promise<ConversationSearchResponse>;
  getSuggestions(userId: string, prefix: string): Promise<SearchSuggestion[]>;
  getSearchHistory(userId: string): Promise<SearchHistory[]>;
  clearSearchHistory(userId: string): Promise<void>;
  indexMessage(message: IndexableMessage): Promise<void>;
  removeFromIndex(messageId: string): Promise<void>;
}

export class AdvancedSearchServiceImpl implements AdvancedSearchService {
  private index: MemorySearchIndex;
  private historyStore: SearchHistoryStore;
  private eventBus: EventBus;
  
  constructor(index: MemorySearchIndex, historyStore: SearchHistoryStore, eventBus: EventBus) {
    this.index = index;
    this.historyStore = historyStore;
    this.eventBus = eventBus;
  }
  
  async search(request: AdvancedSearchRequest): Promise<AdvancedSearchResponse> {
    const startTime = Date.now();
    
    const options: SearchOptions = {
      limit: request.limit ?? 50,
      offset: request.offset ?? 0,
    };
    
    // Apply filters
    if (request.filters) {
      const f = request.filters;
      if (f.dateRange) {
        options.dateRange = {
          from: f.dateRange.from ?? undefined,
          to: f.dateRange.to ?? undefined,
        };
      }
      if (f.messageTypes) options.messageTypes = f.messageTypes;
      if (f.senderIds) options.senderIds = f.senderIds;
      if (f.chatIds) options.chatIds = f.chatIds;
      options.hasMedia = f.hasMedia ?? undefined;
      options.hasLinks = f.hasLinks ?? undefined;
    }
    
    const results = await this.index.search(request.query, options);
    
    // Transform to response format
    const items: SearchResultItem[] = results.map(r => ({
      messageId: r.messageId,
      chatId: r.chatId,
      senderId: r.senderId,
      senderName: r.senderName,
      content: r.content,
      snippet: r.snippet,
      type: r.type as 'text' | 'media' | 'document' | 'link',
      timestamp: r.timestamp,
      relevanceScore: r.relevanceScore,
      highlights: r.highlights,
    }));
    
    // Record search
    if (request.userId) {
      await this.historyStore.addSearch(request.userId, request.query);
      this.index.recordSearch(request.userId, request.query);
    }
    
    const response: AdvancedSearchResponse = {
      query: request.query,
      results: items,
      totalCount: items.length,
      offset: request.offset ?? 0,
      hasMore: items.length === (request.limit ?? 50),
      searchTime: Date.now() - startTime,
    };
    
    this.eventBus.publish({
      type: SEARCH_EVENTS.SEARCH_PERFORMED,
      payload: {
        userId: request.userId,
        query: request.query,
        resultCount: items.length,
        searchTime: response.searchTime,
      },
    });
    
    return response;
  }
  
  async searchInConversation(request: ConversationSearchRequest): Promise<ConversationSearchResponse> {
    const results = await this.index.searchInChat(request.chatId, request.query, {
      limit: request.limit,
      offset: request.offset,
    });
    
    return {
      chatId: request.chatId,
      query: request.query,
      results: results.map(r => ({
        messageId: r.messageId,
        chatId: r.chatId,
        senderId: r.senderId,
        senderName: r.senderName,
        content: r.content,
        snippet: r.snippet,
        type: r.type as 'text' | 'media' | 'document' | 'link',
        timestamp: r.timestamp,
        relevanceScore: r.relevanceScore,
        highlights: r.highlights,
      })),
      totalCount: results.length,
      matchPositions: results.map(r => ({
        messageId: r.messageId,
        positions: [],
      })),
    };
  }
  
  async getSuggestions(userId: string, prefix: string): Promise<SearchSuggestion[]> {
    const suggestions = await this.index.getSuggestions(prefix, userId);
    
    return suggestions.map((text, index) => ({
      text,
      type: 'history' as const,
      relevance: 1 - index * 0.1,
    }));
  }
  
  async getSearchHistory(userId: string): Promise<SearchHistory[]> {
    return this.historyStore.getHistory(userId);
  }
  
  async clearSearchHistory(userId: string): Promise<void> {
    await this.historyStore.clearHistory(userId);
  }
  
  async indexMessage(message: IndexableMessage): Promise<void> {
    await this.index.indexMessage(message);
  }
  
  async removeFromIndex(messageId: string): Promise<void> {
    await this.index.removeMessage(messageId);
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let searchService: AdvancedSearchService | null = null;

export function getAdvancedSearchService(): AdvancedSearchService {
  if (!searchService) {
    searchService = new AdvancedSearchServiceImpl(
      new MemorySearchIndex(),
      new MemorySearchHistoryStore(),
      getEventBus()
    );
  }
  return searchService;
}
