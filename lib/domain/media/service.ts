// Media Service
// Handles media uploads, processing, thumbnails, and previews

import type { 
  MediaMetadata, 
  MediaUploadRequest, 
  MediaUploadResponse,
  Thumbnail,
  ThumbnailConfig,
  LinkPreview,
  RichMediaPreview,
  VoiceTranscription,
} from '@/lib/contracts/media';
import type { EventBus, DomainEvent } from '@/lib/platform/events/types';
import { createEvent } from '@/lib/platform/events/types';

// ============================================================================
// EVENTS
// ============================================================================

export const MediaEventTypes = {
  MEDIA_UPLOADED: 'MEDIA_UPLOADED',
  MEDIA_PROCESSED: 'MEDIA_PROCESSED',
  MEDIA_FAILED: 'MEDIA_FAILED',
  TRANSCRIPTION_COMPLETED: 'TRANSCRIPTION_COMPLETED',
} as const;

export interface MediaUploadedPayload {
  media: MediaMetadata;
  chatId?: string;
  uploadedBy: string;
}

export interface MediaProcessedPayload {
  mediaId: string;
  thumbnailUrl?: string;
  previewUrl?: string;
  transcription?: VoiceTranscription;
}

export function mediaUploadedEvent(payload: MediaUploadedPayload): DomainEvent<MediaUploadedPayload> {
  return createEvent(MediaEventTypes.MEDIA_UPLOADED, payload);
}

export function mediaProcessedEvent(payload: MediaProcessedPayload): DomainEvent<MediaProcessedPayload> {
  return createEvent(MediaEventTypes.MEDIA_PROCESSED, payload);
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface MediaStore {
  createMedia(media: MediaMetadata): Promise<MediaMetadata>;
  getMedia(mediaId: string): Promise<MediaMetadata | null>;
  updateMedia(mediaId: string, updates: Partial<MediaMetadata>): Promise<MediaMetadata>;
  deleteMedia(mediaId: string): Promise<void>;
  getMediaForChat(chatId: string, type?: string, limit?: number): Promise<MediaMetadata[]>;
}

export interface ThumbnailStore {
  saveThumbnail(thumbnail: Thumbnail): Promise<Thumbnail>;
  getThumbnail(mediaId: string): Promise<Thumbnail | null>;
}

export interface LinkPreviewStore {
  savePreview(url: string, preview: LinkPreview): Promise<LinkPreview>;
  getPreview(url: string): Promise<LinkPreview | null>;
}

export interface TranscriptionStore {
  saveTranscription(transcription: VoiceTranscription): Promise<VoiceTranscription>;
  getTranscription(mediaId: string): Promise<VoiceTranscription | null>;
}

// ============================================================================
// SERVICE
// ============================================================================

export interface InitiateUploadCommand {
  filename: string;
  mimeType: string;
  size: number;
  uploadedBy: string;
  chatId?: string;
}

export interface ProcessMediaCommand {
  mediaId: string;
  generateThumbnail?: boolean;
  transcribeAudio?: boolean;
}

export interface FetchLinkPreviewCommand {
  url: string;
  fetchRichMedia?: boolean;
}

export async function initiateUpload(
  store: MediaStore,
  command: InitiateUploadCommand
): Promise<MediaUploadResponse> {
  const mediaId = `media-${Date.now()}-${Math.random().toString(36).slice(2)}`;
  const timestamp = new Date().toISOString();
  
  // Determine media type from MIME type
  const type = getMediaTypeFromMime(command.mimeType);
  
  const media: MediaMetadata = {
    id: mediaId,
    type,
    mimeType: command.mimeType,
    filename: command.filename,
    size: command.size,
    url: '', // Will be set after upload
    status: 'uploading',
    uploadedAt: timestamp,
  };
  
  await store.createMedia(media);
  
  // Generate pre-signed upload URL (demo implementation)
  const uploadUrl = `/api/media/upload/${mediaId}`;
  const expiresAt = new Date(Date.now() + 3600000).toISOString(); // 1 hour
  
  return {
    mediaId,
    uploadUrl,
    expiresAt,
  };
}

export async function completeUpload(
  store: MediaStore,
  mediaId: string,
  url: string,
  metadata?: { width?: number; height?: number; duration?: number },
  eventBus?: EventBus | null
): Promise<MediaMetadata> {
  const updated = await store.updateMedia(mediaId, {
    url,
    status: 'processing',
    width: metadata?.width,
    height: metadata?.height,
    duration: metadata?.duration,
  });
  
  if (eventBus) {
    await eventBus.publish(mediaUploadedEvent({
      media: updated,
      uploadedBy: '', // Would come from auth context
    }));
  }
  
  return updated;
}

export async function processMedia(
  store: MediaStore,
  thumbnailStore: ThumbnailStore,
  command: ProcessMediaCommand,
  eventBus?: EventBus | null
): Promise<MediaMetadata> {
  const media = await store.getMedia(command.mediaId);
  if (!media) {
    throw new Error('Media not found');
  }
  
  let thumbnailUrl: string | undefined;
  
  // Generate thumbnail for images and videos
  if (command.generateThumbnail && ['image', 'video'].includes(media.type)) {
    thumbnailUrl = await generateThumbnail(media, thumbnailStore);
  }
  
  const updated = await store.updateMedia(command.mediaId, {
    status: 'ready',
    thumbnailUrl,
    processedAt: new Date().toISOString(),
  });
  
  if (eventBus) {
    await eventBus.publish(mediaProcessedEvent({
      mediaId: command.mediaId,
      thumbnailUrl,
    }));
  }
  
  return updated;
}

async function generateThumbnail(
  media: MediaMetadata,
  store: ThumbnailStore
): Promise<string> {
  // Demo implementation - just return placeholder
  const thumbnailUrl = `${media.url}?thumb=true`;
  
  const thumbnail: Thumbnail = {
    mediaId: media.id,
    url: thumbnailUrl,
    width: 200,
    height: 200,
    size: 0,
  };
  
  await store.saveThumbnail(thumbnail);
  return thumbnailUrl;
}

export async function fetchLinkPreview(
  store: LinkPreviewStore,
  url: string
): Promise<LinkPreview | null> {
  // Check cache first
  const cached = await store.getPreview(url);
  if (cached) {
    return cached;
  }
  
  // Demo implementation - would normally fetch and parse Open Graph tags
  const preview: LinkPreview = {
    url,
    title: 'Link Preview',
    description: 'Description would be fetched from the URL',
    fetchedAt: new Date().toISOString(),
  };
  
  await store.savePreview(url, preview);
  return preview;
}

function getMediaTypeFromMime(mimeType: string): MediaMetadata['type'] {
  if (mimeType.startsWith('image/gif')) return 'gif';
  if (mimeType.startsWith('image/')) return 'image';
  if (mimeType.startsWith('video/')) return 'video';
  if (mimeType.startsWith('audio/')) return 'audio';
  if (mimeType === 'application/pdf' || mimeType.startsWith('application/')) return 'document';
  return 'document';
}

export async function getMedia(store: MediaStore, mediaId: string): Promise<MediaMetadata | null> {
  return store.getMedia(mediaId);
}

export async function getMediaForChat(
  store: MediaStore,
  chatId: string,
  type?: string,
  limit?: number
): Promise<MediaMetadata[]> {
  return store.getMediaForChat(chatId, type, limit);
}

// ============================================================================
// IN-MEMORY STORES
// ============================================================================

export class MemoryMediaStore implements MediaStore {
  private media = new Map<string, MediaMetadata>();
  
  async createMedia(media: MediaMetadata): Promise<MediaMetadata> {
    this.media.set(media.id, media);
    return media;
  }
  
  async getMedia(mediaId: string): Promise<MediaMetadata | null> {
    return this.media.get(mediaId) ?? null;
  }
  
  async updateMedia(mediaId: string, updates: Partial<MediaMetadata>): Promise<MediaMetadata> {
    const existing = this.media.get(mediaId);
    if (!existing) throw new Error('Media not found');
    
    const updated = { ...existing, ...updates };
    this.media.set(mediaId, updated);
    return updated;
  }
  
  async deleteMedia(mediaId: string): Promise<void> {
    this.media.delete(mediaId);
  }
  
  async getMediaForChat(chatId: string, type?: string, limit = 50): Promise<MediaMetadata[]> {
    // Demo: return all media (real implementation would filter by chat)
    let results = Array.from(this.media.values());
    if (type) {
      results = results.filter(m => m.type === type);
    }
    return results.slice(0, limit);
  }
}

export class MemoryThumbnailStore implements ThumbnailStore {
  private thumbnails = new Map<string, Thumbnail>();
  
  async saveThumbnail(thumbnail: Thumbnail): Promise<Thumbnail> {
    this.thumbnails.set(thumbnail.mediaId, thumbnail);
    return thumbnail;
  }
  
  async getThumbnail(mediaId: string): Promise<Thumbnail | null> {
    return this.thumbnails.get(mediaId) ?? null;
  }
}

export class MemoryLinkPreviewStore implements LinkPreviewStore {
  private previews = new Map<string, LinkPreview>();
  
  async savePreview(url: string, preview: LinkPreview): Promise<LinkPreview> {
    this.previews.set(url, preview);
    return preview;
  }
  
  async getPreview(url: string): Promise<LinkPreview | null> {
    return this.previews.get(url) ?? null;
  }
}

// Singletons
let mediaStore: MediaStore | null = null;
let thumbnailStore: ThumbnailStore | null = null;
let linkPreviewStore: LinkPreviewStore | null = null;

export function getMediaStore(): MediaStore {
  if (!mediaStore) mediaStore = new MemoryMediaStore();
  return mediaStore;
}

export function getThumbnailStore(): ThumbnailStore {
  if (!thumbnailStore) thumbnailStore = new MemoryThumbnailStore();
  return thumbnailStore;
}

export function getLinkPreviewStore(): LinkPreviewStore {
  if (!linkPreviewStore) linkPreviewStore = new MemoryLinkPreviewStore();
  return linkPreviewStore;
}
