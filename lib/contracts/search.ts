import { z } from 'zod';

// ============================================================================
// ADVANCED SEARCH
// ============================================================================

export const SearchFilterSchema = z.object({
  // Date range
  startDate: z.string().datetime().optional(),
  endDate: z.string().datetime().optional(),
  
  // Participant filters
  fromUserIds: z.array(z.string()).optional(),
  inChatIds: z.array(z.string()).optional(),
  chatTypes: z.array(z.enum(['individual', 'group'])).optional(),
  
  // Content filters
  hasAttachments: z.boolean().optional(),
  hasLinks: z.boolean().optional(),
  hasReactions: z.boolean().optional(),
  hasMentions: z.boolean().optional(),
  mentionedUserIds: z.array(z.string()).optional(),
  
  // Message type filters
  messageTypes: z.array(z.enum([
    'text',
    'image',
    'video',
    'audio',
    'voice',
    'document',
    'sticker',
    'gif',
    'poll',
    'location',
  ])).optional(),
  
  // Attachment filters
  attachmentTypes: z.array(z.enum([
    'image',
    'video',
    'audio',
    'document',
    'pdf',
    'spreadsheet',
    'presentation',
  ])).optional(),
  
  // Status filters
  isStarred: z.boolean().optional(),
  isPinned: z.boolean().optional(),
  isForwarded: z.boolean().optional(),
});

export type SearchFilter = z.infer<typeof SearchFilterSchema>;

export const AdvancedSearchRequestSchema = z.object({
  query: z.string().min(1).max(500),
  filters: SearchFilterSchema.optional(),
  
  // Pagination
  limit: z.number().int().min(1).max(100).optional().default(20),
  offset: z.number().int().min(0).optional().default(0),
  
  // Sorting
  sortBy: z.enum(['relevance', 'date_desc', 'date_asc']).optional().default('relevance'),
  
  // Search scope
  scope: z.enum(['all', 'messages', 'media', 'links', 'documents']).optional().default('all'),
});

export type AdvancedSearchRequest = z.infer<typeof AdvancedSearchRequestSchema>;

export const SearchResultItemSchema = z.object({
  id: z.string().min(1),
  type: z.enum(['message', 'media', 'link', 'document', 'contact']),
  
  // Message info
  messageId: z.string(),
  chatId: z.string(),
  chatName: z.string().optional(),
  senderId: z.string(),
  senderName: z.string().optional(),
  
  // Content
  content: z.string(),
  highlights: z.array(z.object({
    text: z.string(),
    startIndex: z.number(),
    endIndex: z.number(),
  })).optional(),
  
  // Media preview
  mediaUrl: z.string().url().optional(),
  thumbnailUrl: z.string().url().optional(),
  
  // Metadata
  timestamp: z.string().datetime(),
  relevanceScore: z.number().min(0).max(1),
});

export type SearchResultItem = z.infer<typeof SearchResultItemSchema>;

export const AdvancedSearchResponseSchema = z.object({
  query: z.string(),
  results: z.array(SearchResultItemSchema),
  totalCount: z.number().int().min(0),
  
  // Facets for filtering
  facets: z.object({
    chatCounts: z.record(z.string(), z.number()).optional(),
    typeCounts: z.record(z.string(), z.number()).optional(),
    senderCounts: z.record(z.string(), z.number()).optional(),
    dateBuckets: z.array(z.object({
      date: z.string(),
      count: z.number(),
    })).optional(),
  }).optional(),
  
  // Suggestions
  didYouMean: z.string().optional(),
  relatedSearches: z.array(z.string()).optional(),
  
  executionTimeMs: z.number(),
});

export type AdvancedSearchResponse = z.infer<typeof AdvancedSearchResponseSchema>;

// ============================================================================
// CONVERSATION SEARCH
// ============================================================================

export const ConversationSearchRequestSchema = z.object({
  query: z.string().min(1).max(200),
  
  // Search in chat names, participant names
  includeChats: z.boolean().optional().default(true),
  includeContacts: z.boolean().optional().default(true),
  includeGroups: z.boolean().optional().default(true),
  
  limit: z.number().int().min(1).max(50).optional().default(20),
});

export type ConversationSearchRequest = z.infer<typeof ConversationSearchRequestSchema>;

export const ConversationSearchResultSchema = z.object({
  type: z.enum(['chat', 'contact', 'group']),
  id: z.string().min(1),
  name: z.string(),
  avatarUrl: z.string().url().optional(),
  lastMessage: z.string().optional(),
  lastMessageAt: z.string().datetime().optional(),
  matchedField: z.enum(['name', 'message', 'participant']),
  matchHighlight: z.string().optional(),
});

export type ConversationSearchResult = z.infer<typeof ConversationSearchResultSchema>;

// ============================================================================
// SEARCH HISTORY & SUGGESTIONS
// ============================================================================

export const SearchHistoryItemSchema = z.object({
  query: z.string(),
  searchedAt: z.string().datetime(),
  resultCount: z.number().int().min(0),
  filters: SearchFilterSchema.optional(),
});

export type SearchHistoryItem = z.infer<typeof SearchHistoryItemSchema>;

export const SearchSuggestionsRequestSchema = z.object({
  prefix: z.string().min(1).max(100),
  limit: z.number().int().min(1).max(10).optional().default(5),
});

export type SearchSuggestionsRequest = z.infer<typeof SearchSuggestionsRequestSchema>;
