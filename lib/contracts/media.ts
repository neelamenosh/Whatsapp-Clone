import { z } from 'zod';

// ============================================================================
// MEDIA TYPES & SHARING
// ============================================================================

export const MediaTypeSchema = z.enum([
  'image',
  'video',
  'audio',
  'voice',
  'document',
  'sticker',
  'gif',
]);
export type MediaType = z.infer<typeof MediaTypeSchema>;

export const MediaMetadataSchema = z.object({
  id: z.string().min(1),
  type: MediaTypeSchema,
  mimeType: z.string(),
  filename: z.string(),
  size: z.number().int().positive(), // bytes
  
  // Dimensions for images/videos
  width: z.number().int().positive().optional(),
  height: z.number().int().positive().optional(),
  
  // Duration for audio/video
  duration: z.number().positive().optional(), // seconds
  
  // URLs
  url: z.string().url(),
  thumbnailUrl: z.string().url().optional(),
  previewUrl: z.string().url().optional(), // Low-res preview
  
  // Processing status
  status: z.enum(['uploading', 'processing', 'ready', 'failed']),
  
  // Checksums for integrity
  checksum: z.string().optional(),
  
  // Timestamps
  uploadedAt: z.string().datetime(),
  processedAt: z.string().datetime().optional(),
});

export type MediaMetadata = z.infer<typeof MediaMetadataSchema>;

export const MediaUploadRequestSchema = z.object({
  filename: z.string().min(1),
  mimeType: z.string(),
  size: z.number().int().positive(),
  chatId: z.string().min(1).optional(),
});

export type MediaUploadRequest = z.infer<typeof MediaUploadRequestSchema>;

export const MediaUploadResponseSchema = z.object({
  mediaId: z.string().min(1),
  uploadUrl: z.string().url(), // Pre-signed URL for upload
  expiresAt: z.string().datetime(),
});

export type MediaUploadResponse = z.infer<typeof MediaUploadResponseSchema>;

// ============================================================================
// THUMBNAIL GENERATION
// ============================================================================

export const ThumbnailConfigSchema = z.object({
  width: z.number().int().positive().default(200),
  height: z.number().int().positive().default(200),
  quality: z.number().int().min(1).max(100).default(80),
  format: z.enum(['jpeg', 'webp', 'png']).default('webp'),
});

export type ThumbnailConfig = z.infer<typeof ThumbnailConfigSchema>;

export const ThumbnailSchema = z.object({
  mediaId: z.string().min(1),
  url: z.string().url(),
  width: z.number().int().positive(),
  height: z.number().int().positive(),
  size: z.number().int().positive(),
});

export type Thumbnail = z.infer<typeof ThumbnailSchema>;

// ============================================================================
// LINK PREVIEW
// ============================================================================

export const LinkPreviewSchema = z.object({
  url: z.string().url(),
  title: z.string().optional(),
  description: z.string().optional(),
  siteName: z.string().optional(),
  imageUrl: z.string().url().optional(),
  imageWidth: z.number().int().positive().optional(),
  imageHeight: z.number().int().positive().optional(),
  faviconUrl: z.string().url().optional(),
  type: z.enum(['website', 'article', 'video', 'audio', 'rich']).optional(),
  videoUrl: z.string().url().optional(),
  fetchedAt: z.string().datetime(),
});

export type LinkPreview = z.infer<typeof LinkPreviewSchema>;

// ============================================================================
// RICH MEDIA PREVIEW (YouTube, Twitter, Instagram, etc.)
// ============================================================================

export const RichMediaProviderSchema = z.enum([
  'youtube',
  'twitter',
  'instagram',
  'tiktok',
  'spotify',
  'soundcloud',
  'vimeo',
  'twitch',
  'reddit',
  'github',
  'linkedin',
  'facebook',
  'generic',
]);
export type RichMediaProvider = z.infer<typeof RichMediaProviderSchema>;

export const RichMediaPreviewSchema = z.object({
  url: z.string().url(),
  provider: RichMediaProviderSchema,
  embedUrl: z.string().url().optional(),
  embedHtml: z.string().optional(),
  title: z.string().optional(),
  description: z.string().optional(),
  thumbnailUrl: z.string().url().optional(),
  authorName: z.string().optional(),
  authorUrl: z.string().url().optional(),
  duration: z.number().positive().optional(),
  viewCount: z.number().int().optional(),
  publishedAt: z.string().datetime().optional(),
});

export type RichMediaPreview = z.infer<typeof RichMediaPreviewSchema>;

// ============================================================================
// URL SHORTENING (Bitly, TinyURL)
// ============================================================================

export const ShortenedUrlSchema = z.object({
  originalUrl: z.string().url(),
  shortUrl: z.string().url(),
  provider: z.enum(['bitly', 'tinyurl', 'internal']),
  createdAt: z.string().datetime(),
  expiresAt: z.string().datetime().optional(),
  clickCount: z.number().int().optional(),
});

export type ShortenedUrl = z.infer<typeof ShortenedUrlSchema>;

// ============================================================================
// VOICE TRANSCRIPTION
// ============================================================================

export const TranscriptionStatusSchema = z.enum([
  'pending',
  'processing',
  'completed',
  'failed',
]);
export type TranscriptionStatus = z.infer<typeof TranscriptionStatusSchema>;

export const VoiceTranscriptionSchema = z.object({
  mediaId: z.string().min(1),
  messageId: z.string().min(1),
  status: TranscriptionStatusSchema,
  text: z.string().optional(),
  language: z.string().optional(), // ISO 639-1 code
  confidence: z.number().min(0).max(1).optional(),
  words: z.array(z.object({
    word: z.string(),
    startTime: z.number(), // seconds
    endTime: z.number(),
    confidence: z.number().min(0).max(1),
  })).optional(),
  duration: z.number().positive().optional(),
  processedAt: z.string().datetime().optional(),
  error: z.string().optional(),
});

export type VoiceTranscription = z.infer<typeof VoiceTranscriptionSchema>;

// ============================================================================
// DOCUMENT PREVIEW
// ============================================================================

export const DocumentPreviewSchema = z.object({
  mediaId: z.string().min(1),
  pageCount: z.number().int().positive().optional(),
  previewPages: z.array(z.object({
    pageNumber: z.number().int().positive(),
    thumbnailUrl: z.string().url(),
  })).optional(),
  textContent: z.string().optional(), // Extracted text for search
  metadata: z.record(z.string()).optional(), // PDF metadata, etc.
});

export type DocumentPreview = z.infer<typeof DocumentPreviewSchema>;
