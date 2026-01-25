import { z } from 'zod';

// ============================================================================
// REACTIONS
// ============================================================================

export const ReactionSchema = z.object({
  emoji: z.string().min(1).max(8), // Support emoji sequences
  userId: z.string().min(1),
  messageId: z.string().min(1),
  timestamp: z.string().datetime(),
});

export type Reaction = z.infer<typeof ReactionSchema>;

export const AddReactionRequestSchema = z.object({
  messageId: z.string().min(1),
  emoji: z.string().min(1).max(8),
});

export type AddReactionRequest = z.infer<typeof AddReactionRequestSchema>;

// ============================================================================
// MENTIONS
// ============================================================================

export const MentionTypeSchema = z.enum(['user', 'everyone']);
export type MentionType = z.infer<typeof MentionTypeSchema>;

export const MentionSchema = z.object({
  type: MentionTypeSchema,
  userId: z.string().optional(), // null for @everyone
  startIndex: z.number().int().min(0),
  length: z.number().int().min(1),
});

export type Mention = z.infer<typeof MentionSchema>;

// ============================================================================
// THREADED CONVERSATIONS
// ============================================================================

export const ThreadSchema = z.object({
  id: z.string().min(1),
  chatId: z.string().min(1),
  parentMessageId: z.string().min(1),
  participantIds: z.array(z.string()),
  replyCount: z.number().int().min(0),
  lastReplyAt: z.string().datetime().optional(),
  createdAt: z.string().datetime(),
});

export type Thread = z.infer<typeof ThreadSchema>;

// ============================================================================
// QUOTED REPLIES
// ============================================================================

export const QuotedMessageSchema = z.object({
  messageId: z.string().min(1),
  senderId: z.string().min(1),
  senderName: z.string().optional(),
  content: z.string(), // Truncated preview
  type: z.enum(['text', 'image', 'voice', 'location', 'document', 'video', 'sticker', 'poll']),
  mediaUrl: z.string().url().optional(),
});

export type QuotedMessage = z.infer<typeof QuotedMessageSchema>;

// ============================================================================
// MARK AS UNREAD
// ============================================================================

export const UnreadMarkerSchema = z.object({
  chatId: z.string().min(1),
  userId: z.string().min(1),
  markedAt: z.string().datetime(),
  fromMessageId: z.string().min(1).optional(), // Mark unread from this message
});

export type UnreadMarker = z.infer<typeof UnreadMarkerSchema>;

// ============================================================================
// READ RECEIPTS
// ============================================================================

export const ReadReceiptSchema = z.object({
  messageId: z.string().min(1),
  chatId: z.string().min(1),
  userId: z.string().min(1),
  status: z.enum(['delivered', 'read']),
  timestamp: z.string().datetime(),
});

export type ReadReceipt = z.infer<typeof ReadReceiptSchema>;

export const ReadReceiptBatchSchema = z.object({
  chatId: z.string().min(1),
  messageIds: z.array(z.string().min(1)),
  status: z.enum(['delivered', 'read']),
});

export type ReadReceiptBatch = z.infer<typeof ReadReceiptBatchSchema>;

// ============================================================================
// USER PRESENCE
// ============================================================================

export const PresenceStatusSchema = z.enum(['online', 'offline', 'away', 'busy', 'invisible']);
export type PresenceStatus = z.infer<typeof PresenceStatusSchema>;

export const UserPresenceSchema = z.object({
  userId: z.string().min(1),
  status: PresenceStatusSchema,
  lastSeen: z.string().datetime().optional(),
  customStatus: z.string().max(140).optional(),
  deviceType: z.enum(['mobile', 'desktop', 'web']).optional(),
});

export type UserPresence = z.infer<typeof UserPresenceSchema>;

// ============================================================================
// TYPING INDICATORS
// ============================================================================

export const TypingIndicatorSchema = z.object({
  chatId: z.string().min(1),
  userId: z.string().min(1),
  isTyping: z.boolean(),
  timestamp: z.string().datetime(),
});

export type TypingIndicator = z.infer<typeof TypingIndicatorSchema>;

// ============================================================================
// PIN MESSAGE
// ============================================================================

export const PinnedMessageSchema = z.object({
  messageId: z.string().min(1),
  chatId: z.string().min(1),
  pinnedBy: z.string().min(1),
  pinnedAt: z.string().datetime(),
  expiresAt: z.string().datetime().optional(), // Auto-unpin after duration
});

export type PinnedMessage = z.infer<typeof PinnedMessageSchema>;

// ============================================================================
// SAVED MESSAGES
// ============================================================================

export const SavedMessageSchema = z.object({
  id: z.string().min(1),
  userId: z.string().min(1),
  messageId: z.string().min(1),
  chatId: z.string().min(1),
  note: z.string().max(500).optional(),
  tags: z.array(z.string()).optional(),
  savedAt: z.string().datetime(),
});

export type SavedMessage = z.infer<typeof SavedMessageSchema>;

// ============================================================================
// REPORT MESSAGE
// ============================================================================

export const ReportReasonSchema = z.enum([
  'spam',
  'harassment',
  'hate_speech',
  'violence',
  'nudity',
  'false_information',
  'scam',
  'other',
]);
export type ReportReason = z.infer<typeof ReportReasonSchema>;

export const MessageReportSchema = z.object({
  id: z.string().min(1),
  messageId: z.string().min(1),
  chatId: z.string().min(1),
  reporterId: z.string().min(1),
  reportedUserId: z.string().min(1),
  reason: ReportReasonSchema,
  description: z.string().max(1000).optional(),
  status: z.enum(['pending', 'reviewed', 'actioned', 'dismissed']),
  createdAt: z.string().datetime(),
  reviewedAt: z.string().datetime().optional(),
  reviewedBy: z.string().optional(),
});

export type MessageReport = z.infer<typeof MessageReportSchema>;

// ============================================================================
// DISAPPEARING MESSAGES
// ============================================================================

export const DisappearingModeSchema = z.enum(['off', '24h', '7d', '90d']);
export type DisappearingMode = z.infer<typeof DisappearingModeSchema>;

export const DisappearingSettingsSchema = z.object({
  chatId: z.string().min(1),
  mode: DisappearingModeSchema,
  setBy: z.string().min(1),
  setAt: z.string().datetime(),
});

export type DisappearingSettings = z.infer<typeof DisappearingSettingsSchema>;
