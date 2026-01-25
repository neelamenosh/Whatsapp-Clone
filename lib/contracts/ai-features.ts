import { z } from 'zod';

// ============================================================================
// CONVERSATION STARTER (AI-suggested first message)
// ============================================================================

export const ConversationStarterRequestSchema = z.object({
  chatId: z.string().min(1),
  participantIds: z.array(z.string()).min(1),
  context: z.object({
    // Known information about participants
    participantNames: z.record(z.string()).optional(),
    participantInterests: z.record(z.array(z.string())).optional(),
    
    // Previous interaction history
    hasHistoricalMessages: z.boolean().optional(),
    lastMessageDate: z.string().datetime().optional(),
    
    // Context clues
    occasion: z.string().optional(), // birthday, holiday, etc.
    tone: z.enum(['casual', 'formal', 'friendly', 'professional']).optional(),
  }).optional(),
  count: z.number().int().min(1).max(5).optional().default(3),
});

export type ConversationStarterRequest = z.infer<typeof ConversationStarterRequestSchema>;

export const ConversationStarterSchema = z.object({
  id: z.string().min(1),
  text: z.string(),
  category: z.enum(['greeting', 'question', 'compliment', 'topic', 'followup']),
  confidence: z.number().min(0).max(1),
  tone: z.enum(['casual', 'formal', 'friendly', 'professional']),
});

export type ConversationStarter = z.infer<typeof ConversationStarterSchema>;

export const ConversationStarterResponseSchema = z.object({
  chatId: z.string().min(1),
  suggestions: z.array(ConversationStarterSchema),
  generatedAt: z.string().datetime(),
});

export type ConversationStarterResponse = z.infer<typeof ConversationStarterResponseSchema>;

// ============================================================================
// SMART REPLIES (AI-suggested quick responses)
// ============================================================================

export const SmartReplyRequestSchema = z.object({
  chatId: z.string().min(1),
  messageId: z.string().min(1),
  messageContent: z.string(),
  messageType: z.enum(['text', 'image', 'voice', 'location', 'document']),
  
  // Context for better suggestions
  context: z.object({
    // Recent message history (last 5-10 messages)
    recentMessages: z.array(z.object({
      senderId: z.string(),
      content: z.string(),
      timestamp: z.string().datetime(),
    })).optional(),
    
    // User preferences
    userTone: z.enum(['casual', 'formal', 'emoji-heavy', 'brief']).optional(),
    userLanguage: z.string().optional(),
    
    // Relationship context
    chatType: z.enum(['individual', 'group']).optional(),
    isBusinessChat: z.boolean().optional(),
  }).optional(),
  
  count: z.number().int().min(1).max(5).optional().default(3),
});

export type SmartReplyRequest = z.infer<typeof SmartReplyRequestSchema>;

export const SmartReplySchema = z.object({
  id: z.string().min(1),
  text: z.string(),
  type: z.enum([
    'affirmative',  // Yes, Sure, Okay
    'negative',     // No, Not really
    'question',     // Follow-up question
    'emoji',        // Emoji reaction
    'acknowledgment', // Thanks, Got it
    'continuation', // Continuing conversation
  ]),
  confidence: z.number().min(0).max(1),
  isEmoji: z.boolean(), // Pure emoji response
});

export type SmartReply = z.infer<typeof SmartReplySchema>;

export const SmartReplyResponseSchema = z.object({
  messageId: z.string().min(1),
  suggestions: z.array(SmartReplySchema),
  generatedAt: z.string().datetime(),
  expiresAt: z.string().datetime(), // Suggestions may become stale
});

export type SmartReplyResponse = z.infer<typeof SmartReplyResponseSchema>;

// ============================================================================
// CONVERSATION SUMMARY (AI-generated summary)
// ============================================================================

export const ConversationSummaryRequestSchema = z.object({
  chatId: z.string().min(1),
  
  // Time range
  startDate: z.string().datetime().optional(),
  endDate: z.string().datetime().optional(),
  
  // Or message range
  startMessageId: z.string().optional(),
  endMessageId: z.string().optional(),
  
  // Summary preferences
  maxLength: z.enum(['brief', 'moderate', 'detailed']).optional().default('moderate'),
  includeActionItems: z.boolean().optional().default(true),
  includeKeyTopics: z.boolean().optional().default(true),
  includeParticipantHighlights: z.boolean().optional().default(false),
  language: z.string().optional(), // Summarize in specific language
});

export type ConversationSummaryRequest = z.infer<typeof ConversationSummaryRequestSchema>;

export const ActionItemSchema = z.object({
  id: z.string().min(1),
  text: z.string(),
  assignee: z.string().optional(), // Mentioned user
  dueDate: z.string().datetime().optional(), // Extracted from context
  priority: z.enum(['low', 'medium', 'high']).optional(),
  sourceMessageId: z.string().optional(),
});

export type ActionItem = z.infer<typeof ActionItemSchema>;

export const KeyTopicSchema = z.object({
  topic: z.string(),
  messageCount: z.number().int().min(1),
  sentiment: z.enum(['positive', 'neutral', 'negative']).optional(),
  keyPoints: z.array(z.string()),
});

export type KeyTopic = z.infer<typeof KeyTopicSchema>;

export const ParticipantHighlightSchema = z.object({
  userId: z.string().min(1),
  messageCount: z.number().int().min(0),
  keyContributions: z.array(z.string()),
  sentiment: z.enum(['positive', 'neutral', 'negative']).optional(),
});

export type ParticipantHighlight = z.infer<typeof ParticipantHighlightSchema>;

export const ConversationSummarySchema = z.object({
  id: z.string().min(1),
  chatId: z.string().min(1),
  
  // Summary content
  summary: z.string(),
  
  // Extracted information
  actionItems: z.array(ActionItemSchema).optional(),
  keyTopics: z.array(KeyTopicSchema).optional(),
  participantHighlights: z.array(ParticipantHighlightSchema).optional(),
  
  // Decisions and conclusions
  decisions: z.array(z.string()).optional(),
  
  // Metadata
  messageCount: z.number().int().min(1),
  timeRange: z.object({
    start: z.string().datetime(),
    end: z.string().datetime(),
  }),
  
  generatedAt: z.string().datetime(),
  model: z.string().optional(), // AI model used
  confidence: z.number().min(0).max(1).optional(),
});

export type ConversationSummary = z.infer<typeof ConversationSummarySchema>;

// ============================================================================
// AI MODERATION ASSISTANCE
// ============================================================================

export const ModerationCheckRequestSchema = z.object({
  content: z.string(),
  contentType: z.enum(['text', 'image_url']),
  context: z.object({
    chatId: z.string().optional(),
    senderId: z.string().optional(),
    isGroupChat: z.boolean().optional(),
  }).optional(),
});

export type ModerationCheckRequest = z.infer<typeof ModerationCheckRequestSchema>;

export const ModerationResultSchema = z.object({
  id: z.string().min(1),
  
  // Overall verdict
  isFlagged: z.boolean(),
  action: z.enum(['allow', 'warn', 'block', 'review']),
  
  // Category scores
  categories: z.object({
    hate: z.number().min(0).max(1),
    harassment: z.number().min(0).max(1),
    violence: z.number().min(0).max(1),
    sexual: z.number().min(0).max(1),
    selfHarm: z.number().min(0).max(1),
    spam: z.number().min(0).max(1),
    misinformation: z.number().min(0).max(1),
  }),
  
  // Explanation
  reason: z.string().optional(),
  
  checkedAt: z.string().datetime(),
});

export type ModerationResult = z.infer<typeof ModerationResultSchema>;

// ============================================================================
// AI USER PREFERENCES
// ============================================================================

export const AIUserPreferencesSchema = z.object({
  userId: z.string().min(1),
  
  // Feature toggles
  smartRepliesEnabled: z.boolean(),
  conversationStartersEnabled: z.boolean(),
  autoSummaryEnabled: z.boolean(),
  
  // Preferences
  preferredTone: z.enum(['casual', 'formal', 'emoji-heavy', 'brief']).optional(),
  preferredLanguage: z.string().optional(),
  
  // Privacy
  allowContextAnalysis: z.boolean(), // Allow AI to analyze message history
  excludedChats: z.array(z.string()), // Chats where AI features are disabled
  
  updatedAt: z.string().datetime(),
});

export type AIUserPreferences = z.infer<typeof AIUserPreferencesSchema>;
