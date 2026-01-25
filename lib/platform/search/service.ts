// Search service interface for full-text message search.
// Uses event-sourced projections for scalable, eventually-consistent search.

import type { DomainEvent } from '@/lib/platform/events/types';

export interface SearchResult {
  messageId: string;
  chatId: string;
  content: string;
  senderId: string;
  timestamp: string;
  // Highlighted snippets with search terms
  highlights: string[];
  // Relevance score (0-1)
  score: number;
}

export interface SearchQuery {
  tenantId: string;
  userId: string; // For access control
  
  // Query text (supports quoted phrases, operators)
  query: string;
  
  // Filters
  filters?: {
    chatIds?: string[];
    senderIds?: string[];
    startDate?: Date;
    endDate?: Date;
    hasMedia?: boolean;
    messageType?: ('text' | 'image' | 'video' | 'document' | 'voice')[];
  };
  
  // Pagination
  limit?: number;
  offset?: number;
  
  // Sorting
  sortBy?: 'relevance' | 'timestamp';
  sortOrder?: 'asc' | 'desc';
}

export interface SearchQueryResult {
  results: SearchResult[];
  totalCount: number;
  query: string;
  executionTimeMs: number;
}

export interface SearchService {
  /**
   * Search messages.
   */
  search(query: SearchQuery): Promise<SearchQueryResult>;
  
  /**
   * Index a message (called when message is created).
   */
  indexMessage(event: DomainEvent): Promise<void>;
  
  /**
   * Remove a message from the index (called when deleted).
   */
  removeMessage(tenantId: string, messageId: string): Promise<void>;
  
  /**
   * Get search suggestions/autocomplete.
   */
  getSuggestions(tenantId: string, prefix: string, limit?: number): Promise<string[]>;
}

// In-memory search for demo/development
// Production would use Elasticsearch, Azure Cognitive Search, etc.
export class MemorySearchService implements SearchService {
  private index: Map<string, IndexedMessage> = new Map();
  
  async search(query: SearchQuery): Promise<SearchQueryResult> {
    const startTime = Date.now();
    const terms = this.tokenize(query.query.toLowerCase());
    
    // Filter messages user has access to (simplified - just filter by tenantId)
    let messages = Array.from(this.index.values())
      .filter(m => m.tenantId === query.tenantId);
    
    // Apply filters
    if (query.filters?.chatIds?.length) {
      messages = messages.filter(m => query.filters!.chatIds!.includes(m.chatId));
    }
    if (query.filters?.senderIds?.length) {
      messages = messages.filter(m => query.filters!.senderIds!.includes(m.senderId));
    }
    if (query.filters?.startDate) {
      messages = messages.filter(m => new Date(m.timestamp) >= query.filters!.startDate!);
    }
    if (query.filters?.endDate) {
      messages = messages.filter(m => new Date(m.timestamp) <= query.filters!.endDate!);
    }
    
    // Score by term matches
    const scored = messages.map(m => {
      const contentTerms = this.tokenize(m.content.toLowerCase());
      const matchCount = terms.filter(t => 
        contentTerms.some(ct => ct.includes(t))
      ).length;
      const score = terms.length > 0 ? matchCount / terms.length : 0;
      
      return { message: m, score };
    }).filter(s => s.score > 0);
    
    // Sort
    if (query.sortBy === 'timestamp') {
      scored.sort((a, b) => {
        const cmp = a.message.timestamp.localeCompare(b.message.timestamp);
        return query.sortOrder === 'asc' ? cmp : -cmp;
      });
    } else {
      // Sort by relevance (score descending)
      scored.sort((a, b) => b.score - a.score);
    }
    
    // Paginate
    const offset = query.offset ?? 0;
    const limit = query.limit ?? 20;
    const page = scored.slice(offset, offset + limit);
    
    // Build results with highlights
    const results: SearchResult[] = page.map(({ message, score }) => ({
      messageId: message.messageId,
      chatId: message.chatId,
      content: message.content,
      senderId: message.senderId,
      timestamp: message.timestamp,
      highlights: this.generateHighlights(message.content, terms),
      score,
    }));
    
    return {
      results,
      totalCount: scored.length,
      query: query.query,
      executionTimeMs: Date.now() - startTime,
    };
  }
  
  async indexMessage(event: DomainEvent): Promise<void> {
    if (event.eventType !== 'MESSAGE_CREATED') return;
    
    const payload = event.payload as any;
    const indexed: IndexedMessage = {
      messageId: payload.message?.id ?? payload.messageId,
      chatId: payload.message?.chatId ?? payload.chatId,
      content: payload.message?.content ?? payload.content ?? '',
      senderId: payload.message?.senderId ?? payload.senderId,
      timestamp: payload.message?.timestamp ?? payload.timestamp,
      tenantId: event.tenantId ?? 'default',
    };
    
    this.index.set(indexed.messageId, indexed);
  }
  
  async removeMessage(_tenantId: string, messageId: string): Promise<void> {
    this.index.delete(messageId);
  }
  
  async getSuggestions(tenantId: string, prefix: string, limit = 10): Promise<string[]> {
    const prefixLower = prefix.toLowerCase();
    const words = new Set<string>();
    
    for (const msg of this.index.values()) {
      if (msg.tenantId !== tenantId) continue;
      
      for (const word of this.tokenize(msg.content)) {
        if (word.toLowerCase().startsWith(prefixLower)) {
          words.add(word);
          if (words.size >= limit) break;
        }
      }
      if (words.size >= limit) break;
    }
    
    return Array.from(words);
  }
  
  private tokenize(text: string): string[] {
    return text.split(/\s+/).filter(w => w.length > 1);
  }
  
  private generateHighlights(content: string, terms: string[]): string[] {
    const highlights: string[] = [];
    const words = content.split(/\s+/);
    
    for (let i = 0; i < words.length; i++) {
      const wordLower = words[i].toLowerCase();
      if (terms.some(t => wordLower.includes(t))) {
        // Get context: 3 words before and after
        const start = Math.max(0, i - 3);
        const end = Math.min(words.length, i + 4);
        const snippet = words.slice(start, end).join(' ');
        highlights.push(`...${snippet}...`);
      }
    }
    
    return highlights.slice(0, 3); // Max 3 highlights
  }
  
  // Testing helpers
  clear(): void {
    this.index.clear();
  }
  
  getIndexSize(): number {
    return this.index.size;
  }
}

interface IndexedMessage {
  messageId: string;
  chatId: string;
  content: string;
  senderId: string;
  timestamp: string;
  tenantId: string;
}

// Singleton
let searchService: SearchService | null = null;

export function getSearchService(): SearchService {
  if (!searchService) {
    searchService = new MemorySearchService();
  }
  return searchService;
}
