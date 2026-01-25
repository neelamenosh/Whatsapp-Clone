// Stickers and GIFs Service
// Handles sticker packs, custom stickers, and GIF search

import type {
  Sticker,
  StickerPack,
  GifSearchRequest,
  GifSearchResponse,
  GifItem,
} from '@/lib/contracts/engagement';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const STICKER_EVENTS = {
  STICKER_SENT: 'sticker:sent',
  STICKER_PACK_ADDED: 'sticker:pack_added',
  STICKER_PACK_REMOVED: 'sticker:pack_removed',
  STICKER_FAVORITED: 'sticker:favorited',
  STICKER_UNFAVORITED: 'sticker:unfavorited',
} as const;

export const GIF_EVENTS = {
  GIF_SENT: 'gif:sent',
  GIF_FAVORITED: 'gif:favorited',
} as const;

// ============================================================================
// GIF PROVIDER INTERFACE
// ============================================================================

export interface GifProvider {
  search(query: string, options?: GifSearchOptions): Promise<GifSearchResult>;
  getTrending(options?: GifSearchOptions): Promise<GifSearchResult>;
  getById(id: string): Promise<GifItem | null>;
}

export interface GifSearchOptions {
  limit?: number;
  offset?: number;
  rating?: 'g' | 'pg' | 'pg-13' | 'r';
  language?: string;
}

export interface GifSearchResult {
  items: GifItem[];
  totalCount: number;
  offset: number;
  hasMore: boolean;
}

// ============================================================================
// DEMO GIF PROVIDER
// ============================================================================

export class DemoGifProvider implements GifProvider {
  private demoGifs: GifItem[] = [
    {
      id: 'gif-1',
      url: 'https://media.giphy.com/media/demo1/giphy.gif',
      previewUrl: 'https://media.giphy.com/media/demo1/200w.gif',
      thumbnailUrl: 'https://media.giphy.com/media/demo1/100w.gif',
      width: 480,
      height: 270,
      provider: 'giphy',
      title: 'Demo GIF 1',
    },
    {
      id: 'gif-2',
      url: 'https://media.giphy.com/media/demo2/giphy.gif',
      previewUrl: 'https://media.giphy.com/media/demo2/200w.gif',
      thumbnailUrl: 'https://media.giphy.com/media/demo2/100w.gif',
      width: 480,
      height: 360,
      provider: 'giphy',
      title: 'Demo GIF 2',
    },
  ];
  
  async search(query: string, options?: GifSearchOptions): Promise<GifSearchResult> {
    const limit = options?.limit ?? 20;
    const offset = options?.offset ?? 0;
    
    return {
      items: this.demoGifs.slice(offset, offset + limit),
      totalCount: this.demoGifs.length,
      offset,
      hasMore: offset + limit < this.demoGifs.length,
    };
  }
  
  async getTrending(options?: GifSearchOptions): Promise<GifSearchResult> {
    return this.search('trending', options);
  }
  
  async getById(id: string): Promise<GifItem | null> {
    return this.demoGifs.find(g => g.id === id) ?? null;
  }
}

// ============================================================================
// STICKER STORE INTERFACE
// ============================================================================

export interface StickerStore {
  // Sticker Packs
  createPack(pack: Omit<StickerPack, 'id'>): Promise<StickerPack>;
  getPack(packId: string): Promise<StickerPack | null>;
  getAvailablePacks(): Promise<StickerPack[]>;
  getUserPacks(userId: string): Promise<StickerPack[]>;
  addPackToUser(userId: string, packId: string): Promise<void>;
  removePackFromUser(userId: string, packId: string): Promise<void>;
  
  // Stickers
  createSticker(sticker: Omit<Sticker, 'id'>): Promise<Sticker>;
  getSticker(stickerId: string): Promise<Sticker | null>;
  getPackStickers(packId: string): Promise<Sticker[]>;
  
  // Favorites
  addFavorite(userId: string, stickerId: string): Promise<void>;
  removeFavorite(userId: string, stickerId: string): Promise<void>;
  getFavorites(userId: string): Promise<string[]>;
  
  // Recent
  addRecent(userId: string, stickerId: string): Promise<void>;
  getRecent(userId: string, limit?: number): Promise<string[]>;
  
  // Custom stickers
  createCustomSticker(userId: string, sticker: Omit<Sticker, 'id' | 'packId'>): Promise<Sticker>;
  getCustomStickers(userId: string): Promise<Sticker[]>;
}

// ============================================================================
// IN-MEMORY STICKER STORE
// ============================================================================

export class MemoryStickerStore implements StickerStore {
  private packs = new Map<string, StickerPack>();
  private stickers = new Map<string, Sticker>();
  private userPacks = new Map<string, Set<string>>(); // userId -> packIds
  private favorites = new Map<string, Set<string>>(); // userId -> stickerIds
  private recent = new Map<string, string[]>(); // userId -> stickerIds (ordered)
  private customStickers = new Map<string, Sticker[]>(); // userId -> stickers
  
  async createPack(data: Omit<StickerPack, 'id'>): Promise<StickerPack> {
    const id = `pack-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const pack: StickerPack = { id, ...data };
    this.packs.set(id, pack);
    return pack;
  }
  
  async getPack(packId: string): Promise<StickerPack | null> {
    return this.packs.get(packId) ?? null;
  }
  
  async getAvailablePacks(): Promise<StickerPack[]> {
    return Array.from(this.packs.values());
  }
  
  async getUserPacks(userId: string): Promise<StickerPack[]> {
    const packIds = this.userPacks.get(userId) ?? new Set();
    return Array.from(packIds)
      .map(id => this.packs.get(id))
      .filter((p): p is StickerPack => p !== undefined);
  }
  
  async addPackToUser(userId: string, packId: string): Promise<void> {
    if (!this.userPacks.has(userId)) {
      this.userPacks.set(userId, new Set());
    }
    this.userPacks.get(userId)!.add(packId);
  }
  
  async removePackFromUser(userId: string, packId: string): Promise<void> {
    this.userPacks.get(userId)?.delete(packId);
  }
  
  async createSticker(data: Omit<Sticker, 'id'>): Promise<Sticker> {
    const id = `sticker-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const sticker: Sticker = { id, ...data };
    this.stickers.set(id, sticker);
    return sticker;
  }
  
  async getSticker(stickerId: string): Promise<Sticker | null> {
    return this.stickers.get(stickerId) ?? null;
  }
  
  async getPackStickers(packId: string): Promise<Sticker[]> {
    return Array.from(this.stickers.values())
      .filter(s => s.packId === packId);
  }
  
  async addFavorite(userId: string, stickerId: string): Promise<void> {
    if (!this.favorites.has(userId)) {
      this.favorites.set(userId, new Set());
    }
    this.favorites.get(userId)!.add(stickerId);
  }
  
  async removeFavorite(userId: string, stickerId: string): Promise<void> {
    this.favorites.get(userId)?.delete(stickerId);
  }
  
  async getFavorites(userId: string): Promise<string[]> {
    return Array.from(this.favorites.get(userId) ?? []);
  }
  
  async addRecent(userId: string, stickerId: string): Promise<void> {
    if (!this.recent.has(userId)) {
      this.recent.set(userId, []);
    }
    const recentList = this.recent.get(userId)!;
    // Remove if already exists
    const idx = recentList.indexOf(stickerId);
    if (idx > -1) recentList.splice(idx, 1);
    // Add to front
    recentList.unshift(stickerId);
    // Keep max 50
    if (recentList.length > 50) recentList.pop();
  }
  
  async getRecent(userId: string, limit: number = 20): Promise<string[]> {
    return (this.recent.get(userId) ?? []).slice(0, limit);
  }
  
  async createCustomSticker(
    userId: string,
    data: Omit<Sticker, 'id' | 'packId'>
  ): Promise<Sticker> {
    const id = `custom-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const sticker: Sticker = { id, packId: `custom-${userId}`, ...data };
    
    if (!this.customStickers.has(userId)) {
      this.customStickers.set(userId, []);
    }
    this.customStickers.get(userId)!.push(sticker);
    this.stickers.set(id, sticker);
    
    return sticker;
  }
  
  async getCustomStickers(userId: string): Promise<Sticker[]> {
    return this.customStickers.get(userId) ?? [];
  }
}

// ============================================================================
// SERVICE
// ============================================================================

export interface StickerGifService {
  // Sticker Packs
  getAvailablePacks(): Promise<StickerPack[]>;
  getUserPacks(userId: string): Promise<StickerPack[]>;
  addPackToLibrary(userId: string, packId: string): Promise<void>;
  removePackFromLibrary(userId: string, packId: string): Promise<void>;
  getPackStickers(packId: string): Promise<Sticker[]>;
  
  // Sticker Usage
  sendSticker(userId: string, chatId: string, stickerId: string): Promise<Sticker>;
  getFavoriteStickers(userId: string): Promise<Sticker[]>;
  getRecentStickers(userId: string): Promise<Sticker[]>;
  favoriteSticker(userId: string, stickerId: string): Promise<void>;
  unfavoriteSticker(userId: string, stickerId: string): Promise<void>;
  
  // Custom Stickers
  createCustomSticker(
    userId: string,
    imageUrl: string,
    emoji: string
  ): Promise<Sticker>;
  getCustomStickers(userId: string): Promise<Sticker[]>;
  
  // GIFs
  searchGifs(request: GifSearchRequest): Promise<GifSearchResponse>;
  getTrendingGifs(limit?: number): Promise<GifSearchResponse>;
  sendGif(userId: string, chatId: string, gifId: string): Promise<GifItem | null>;
}

export class StickerGifServiceImpl implements StickerGifService {
  private store: StickerStore;
  private gifProvider: GifProvider;
  private eventBus: EventBus;
  
  constructor(store: StickerStore, gifProvider: GifProvider, eventBus: EventBus) {
    this.store = store;
    this.gifProvider = gifProvider;
    this.eventBus = eventBus;
  }
  
  async getAvailablePacks(): Promise<StickerPack[]> {
    return this.store.getAvailablePacks();
  }
  
  async getUserPacks(userId: string): Promise<StickerPack[]> {
    return this.store.getUserPacks(userId);
  }
  
  async addPackToLibrary(userId: string, packId: string): Promise<void> {
    await this.store.addPackToUser(userId, packId);
    const pack = await this.store.getPack(packId);
    
    this.eventBus.publish({
      type: STICKER_EVENTS.STICKER_PACK_ADDED,
      payload: { userId, packId, pack },
    });
  }
  
  async removePackFromLibrary(userId: string, packId: string): Promise<void> {
    await this.store.removePackFromUser(userId, packId);
    
    this.eventBus.publish({
      type: STICKER_EVENTS.STICKER_PACK_REMOVED,
      payload: { userId, packId },
    });
  }
  
  async getPackStickers(packId: string): Promise<Sticker[]> {
    return this.store.getPackStickers(packId);
  }
  
  async sendSticker(userId: string, chatId: string, stickerId: string): Promise<Sticker> {
    const sticker = await this.store.getSticker(stickerId);
    if (!sticker) {
      throw new Error('Sticker not found');
    }
    
    await this.store.addRecent(userId, stickerId);
    
    this.eventBus.publish({
      type: STICKER_EVENTS.STICKER_SENT,
      payload: { userId, chatId, sticker },
    });
    
    return sticker;
  }
  
  async getFavoriteStickers(userId: string): Promise<Sticker[]> {
    const ids = await this.store.getFavorites(userId);
    const stickers: Sticker[] = [];
    
    for (const id of ids) {
      const sticker = await this.store.getSticker(id);
      if (sticker) stickers.push(sticker);
    }
    
    return stickers;
  }
  
  async getRecentStickers(userId: string): Promise<Sticker[]> {
    const ids = await this.store.getRecent(userId);
    const stickers: Sticker[] = [];
    
    for (const id of ids) {
      const sticker = await this.store.getSticker(id);
      if (sticker) stickers.push(sticker);
    }
    
    return stickers;
  }
  
  async favoriteSticker(userId: string, stickerId: string): Promise<void> {
    await this.store.addFavorite(userId, stickerId);
    
    this.eventBus.publish({
      type: STICKER_EVENTS.STICKER_FAVORITED,
      payload: { userId, stickerId },
    });
  }
  
  async unfavoriteSticker(userId: string, stickerId: string): Promise<void> {
    await this.store.removeFavorite(userId, stickerId);
    
    this.eventBus.publish({
      type: STICKER_EVENTS.STICKER_UNFAVORITED,
      payload: { userId, stickerId },
    });
  }
  
  async createCustomSticker(
    userId: string,
    imageUrl: string,
    emoji: string
  ): Promise<Sticker> {
    return this.store.createCustomSticker(userId, {
      imageUrl,
      emoji,
      isAnimated: false,
    });
  }
  
  async getCustomStickers(userId: string): Promise<Sticker[]> {
    return this.store.getCustomStickers(userId);
  }
  
  async searchGifs(request: GifSearchRequest): Promise<GifSearchResponse> {
    const result = await this.gifProvider.search(request.query, {
      limit: request.limit,
      offset: request.offset,
      rating: request.rating,
    });
    
    return {
      results: result.items,
      query: request.query,
      totalCount: result.totalCount,
      offset: result.offset,
    };
  }
  
  async getTrendingGifs(limit: number = 20): Promise<GifSearchResponse> {
    const result = await this.gifProvider.getTrending({ limit });
    
    return {
      results: result.items,
      query: 'trending',
      totalCount: result.totalCount,
      offset: 0,
    };
  }
  
  async sendGif(userId: string, chatId: string, gifId: string): Promise<GifItem | null> {
    const gif = await this.gifProvider.getById(gifId);
    if (!gif) return null;
    
    this.eventBus.publish({
      type: GIF_EVENTS.GIF_SENT,
      payload: { userId, chatId, gif },
    });
    
    return gif;
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let stickerGifService: StickerGifService | null = null;

export function getStickerGifService(): StickerGifService {
  if (!stickerGifService) {
    stickerGifService = new StickerGifServiceImpl(
      new MemoryStickerStore(),
      new DemoGifProvider(),
      getEventBus()
    );
  }
  return stickerGifService;
}
