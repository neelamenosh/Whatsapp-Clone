import { z } from 'zod';

// ============================================================================
// POLLS
// ============================================================================

export const PollOptionSchema = z.object({
  id: z.string().min(1),
  text: z.string().min(1).max(200),
  voterIds: z.array(z.string()),
  voteCount: z.number().int().min(0),
});

export type PollOption = z.infer<typeof PollOptionSchema>;

export const PollSchema = z.object({
  id: z.string().min(1),
  messageId: z.string().min(1),
  chatId: z.string().min(1),
  creatorId: z.string().min(1),
  question: z.string().min(1).max(500),
  options: z.array(PollOptionSchema).min(2).max(12),
  
  // Settings
  allowMultipleVotes: z.boolean(),
  allowAddOptions: z.boolean(), // Allow participants to add options
  isAnonymous: z.boolean(),
  showResultsBeforeVote: z.boolean(),
  
  // State
  totalVotes: z.number().int().min(0),
  status: z.enum(['active', 'closed']),
  
  // Timestamps
  createdAt: z.string().datetime(),
  closesAt: z.string().datetime().optional(),
  closedAt: z.string().datetime().optional(),
});

export type Poll = z.infer<typeof PollSchema>;

export const CreatePollRequestSchema = z.object({
  chatId: z.string().min(1),
  question: z.string().min(1).max(500),
  options: z.array(z.string().min(1).max(200)).min(2).max(12),
  allowMultipleVotes: z.boolean().optional().default(false),
  allowAddOptions: z.boolean().optional().default(false),
  isAnonymous: z.boolean().optional().default(false),
  closesAt: z.string().datetime().optional(),
});

export type CreatePollRequest = z.infer<typeof CreatePollRequestSchema>;

export const VotePollRequestSchema = z.object({
  pollId: z.string().min(1),
  optionIds: z.array(z.string().min(1)).min(1),
});

export type VotePollRequest = z.infer<typeof VotePollRequestSchema>;

// ============================================================================
// STICKERS
// ============================================================================

export const StickerSchema = z.object({
  id: z.string().min(1),
  packId: z.string().min(1),
  emoji: z.string().optional(), // Associated emoji
  url: z.string().url(),
  thumbnailUrl: z.string().url().optional(),
  width: z.number().int().positive(),
  height: z.number().int().positive(),
  isAnimated: z.boolean(),
  format: z.enum(['png', 'webp', 'gif', 'lottie']),
});

export type Sticker = z.infer<typeof StickerSchema>;

export const StickerPackSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1).max(100),
  publisher: z.string().max(100).optional(),
  thumbnailUrl: z.string().url(),
  stickers: z.array(StickerSchema),
  isAnimated: z.boolean(),
  isOfficial: z.boolean(),
  installCount: z.number().int().optional(),
  createdAt: z.string().datetime(),
});

export type StickerPack = z.infer<typeof StickerPackSchema>;

export const UserStickerPacksSchema = z.object({
  userId: z.string().min(1),
  installedPackIds: z.array(z.string()),
  recentStickerIds: z.array(z.string()).max(50),
  favoriteStickerIds: z.array(z.string()).max(100),
});

export type UserStickerPacks = z.infer<typeof UserStickerPacksSchema>;

// ============================================================================
// GIPHY / TENOR GIF INTEGRATION
// ============================================================================

export const GifSchema = z.object({
  id: z.string().min(1),
  provider: z.enum(['giphy', 'tenor']),
  title: z.string().optional(),
  url: z.string().url(),
  previewUrl: z.string().url(),
  thumbnailUrl: z.string().url(),
  width: z.number().int().positive(),
  height: z.number().int().positive(),
  size: z.number().int().positive().optional(),
  mp4Url: z.string().url().optional(), // For efficient playback
});

export type Gif = z.infer<typeof GifSchema>;

export const GifSearchRequestSchema = z.object({
  query: z.string().min(1).max(100),
  provider: z.enum(['giphy', 'tenor']).optional(),
  limit: z.number().int().min(1).max(50).optional().default(25),
  offset: z.number().int().min(0).optional().default(0),
  rating: z.enum(['g', 'pg', 'pg-13', 'r']).optional().default('pg-13'),
});

export type GifSearchRequest = z.infer<typeof GifSearchRequestSchema>;

// ============================================================================
// REMINDERS
// ============================================================================

export const ReminderSchema = z.object({
  id: z.string().min(1),
  userId: z.string().min(1),
  
  // What to remind about
  type: z.enum(['message', 'custom']),
  messageId: z.string().optional(), // For message reminders
  chatId: z.string().optional(),
  customText: z.string().max(500).optional(),
  
  // When to remind
  remindAt: z.string().datetime(),
  
  // Recurrence
  recurrence: z.enum(['none', 'daily', 'weekly', 'monthly']).optional(),
  
  // State
  status: z.enum(['pending', 'sent', 'dismissed']),
  sentAt: z.string().datetime().optional(),
  
  createdAt: z.string().datetime(),
});

export type Reminder = z.infer<typeof ReminderSchema>;

export const CreateReminderRequestSchema = z.object({
  type: z.enum(['message', 'custom']),
  messageId: z.string().optional(),
  chatId: z.string().optional(),
  customText: z.string().max(500).optional(),
  remindAt: z.string().datetime(),
  recurrence: z.enum(['none', 'daily', 'weekly', 'monthly']).optional(),
});

export type CreateReminderRequest = z.infer<typeof CreateReminderRequestSchema>;

// ============================================================================
// MESSAGE SHORTCUTS
// ============================================================================

export const MessageShortcutSchema = z.object({
  id: z.string().min(1),
  userId: z.string().min(1),
  trigger: z.string().min(1).max(50), // e.g., "!hello"
  content: z.string().min(1).max(2000),
  type: z.enum(['text', 'template']), // Template supports variables
  variables: z.array(z.string()).optional(), // For templates: {name}, {date}, etc.
  useCount: z.number().int().min(0),
  createdAt: z.string().datetime(),
  updatedAt: z.string().datetime(),
});

export type MessageShortcut = z.infer<typeof MessageShortcutSchema>;

// ============================================================================
// MESSAGE TRANSLATION
// ============================================================================

export const TranslationSchema = z.object({
  messageId: z.string().min(1),
  originalText: z.string(),
  originalLanguage: z.string(), // ISO 639-1
  translatedText: z.string(),
  targetLanguage: z.string(),
  confidence: z.number().min(0).max(1).optional(),
  translatedAt: z.string().datetime(),
});

export type Translation = z.infer<typeof TranslationSchema>;

export const TranslateRequestSchema = z.object({
  messageId: z.string().min(1),
  targetLanguage: z.string().min(2).max(5),
});

export type TranslateRequest = z.infer<typeof TranslateRequestSchema>;

export const UserTranslationSettingsSchema = z.object({
  userId: z.string().min(1),
  autoTranslate: z.boolean(),
  preferredLanguage: z.string(),
  excludedLanguages: z.array(z.string()), // Don't auto-translate these
  excludedChats: z.array(z.string()), // Don't auto-translate in these chats
});

export type UserTranslationSettings = z.infer<typeof UserTranslationSettingsSchema>;
