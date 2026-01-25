// Message Shortcuts Service
// Handles custom text shortcuts/quick replies

import type { MessageShortcut } from '@/lib/contracts/engagement';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const SHORTCUT_EVENTS = {
  SHORTCUT_CREATED: 'shortcut:created',
  SHORTCUT_UPDATED: 'shortcut:updated',
  SHORTCUT_DELETED: 'shortcut:deleted',
  SHORTCUT_USED: 'shortcut:used',
} as const;

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface ShortcutStore {
  create(shortcut: Omit<MessageShortcut, 'id'>): Promise<MessageShortcut>;
  get(shortcutId: string): Promise<MessageShortcut | null>;
  getByUser(userId: string): Promise<MessageShortcut[]>;
  getByTrigger(userId: string, trigger: string): Promise<MessageShortcut | null>;
  update(shortcutId: string, updates: Partial<MessageShortcut>): Promise<MessageShortcut | null>;
  delete(shortcutId: string): Promise<boolean>;
  incrementUsage(shortcutId: string): Promise<void>;
  search(userId: string, query: string): Promise<MessageShortcut[]>;
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryShortcutStore implements ShortcutStore {
  private shortcuts = new Map<string, MessageShortcut>();
  
  async create(data: Omit<MessageShortcut, 'id'>): Promise<MessageShortcut> {
    const id = `shortcut-${Date.now()}-${Math.random().toString(36).slice(2)}`;
    const shortcut: MessageShortcut = {
      id,
      ...data,
    };
    
    this.shortcuts.set(id, shortcut);
    return shortcut;
  }
  
  async get(shortcutId: string): Promise<MessageShortcut | null> {
    return this.shortcuts.get(shortcutId) ?? null;
  }
  
  async getByUser(userId: string): Promise<MessageShortcut[]> {
    return Array.from(this.shortcuts.values())
      .filter(s => s.userId === userId)
      .sort((a, b) => (b.usageCount ?? 0) - (a.usageCount ?? 0));
  }
  
  async getByTrigger(userId: string, trigger: string): Promise<MessageShortcut | null> {
    const normalized = trigger.toLowerCase();
    return Array.from(this.shortcuts.values())
      .find(s => s.userId === userId && s.trigger.toLowerCase() === normalized) ?? null;
  }
  
  async update(shortcutId: string, updates: Partial<MessageShortcut>): Promise<MessageShortcut | null> {
    const existing = this.shortcuts.get(shortcutId);
    if (!existing) return null;
    
    const updated: MessageShortcut = { ...existing, ...updates, id: shortcutId };
    this.shortcuts.set(shortcutId, updated);
    return updated;
  }
  
  async delete(shortcutId: string): Promise<boolean> {
    return this.shortcuts.delete(shortcutId);
  }
  
  async incrementUsage(shortcutId: string): Promise<void> {
    const existing = this.shortcuts.get(shortcutId);
    if (existing) {
      existing.usageCount = (existing.usageCount ?? 0) + 1;
      existing.lastUsedAt = new Date().toISOString();
    }
  }
  
  async search(userId: string, query: string): Promise<MessageShortcut[]> {
    const lowerQuery = query.toLowerCase();
    return Array.from(this.shortcuts.values())
      .filter(s => 
        s.userId === userId && (
          s.trigger.toLowerCase().includes(lowerQuery) ||
          s.expansion.toLowerCase().includes(lowerQuery) ||
          s.name?.toLowerCase().includes(lowerQuery) ||
          s.category?.toLowerCase().includes(lowerQuery)
        )
      );
  }
}

// ============================================================================
// SERVICE
// ============================================================================

export interface ShortcutService {
  createShortcut(
    userId: string,
    trigger: string,
    expansion: string,
    options?: {
      name?: string;
      category?: string;
      isGlobal?: boolean;
    }
  ): Promise<MessageShortcut>;
  
  getShortcut(shortcutId: string): Promise<MessageShortcut | null>;
  getUserShortcuts(userId: string): Promise<MessageShortcut[]>;
  updateShortcut(shortcutId: string, updates: Partial<MessageShortcut>): Promise<MessageShortcut | null>;
  deleteShortcut(shortcutId: string): Promise<boolean>;
  
  // Expansion
  expandText(userId: string, text: string): Promise<string>;
  findMatchingShortcut(userId: string, trigger: string): Promise<MessageShortcut | null>;
  searchShortcuts(userId: string, query: string): Promise<MessageShortcut[]>;
}

export class ShortcutServiceImpl implements ShortcutService {
  private store: ShortcutStore;
  private eventBus: EventBus;
  
  constructor(store: ShortcutStore, eventBus: EventBus) {
    this.store = store;
    this.eventBus = eventBus;
  }
  
  async createShortcut(
    userId: string,
    trigger: string,
    expansion: string,
    options?: {
      name?: string;
      category?: string;
      isGlobal?: boolean;
    }
  ): Promise<MessageShortcut> {
    // Check for duplicate trigger
    const existing = await this.store.getByTrigger(userId, trigger);
    if (existing) {
      throw new Error(`Shortcut with trigger "${trigger}" already exists`);
    }
    
    const shortcut = await this.store.create({
      userId,
      trigger,
      expansion,
      name: options?.name,
      category: options?.category,
      isGlobal: options?.isGlobal ?? false,
      usageCount: 0,
      createdAt: new Date().toISOString(),
    });
    
    this.eventBus.publish({
      type: SHORTCUT_EVENTS.SHORTCUT_CREATED,
      payload: shortcut,
    });
    
    return shortcut;
  }
  
  async getShortcut(shortcutId: string): Promise<MessageShortcut | null> {
    return this.store.get(shortcutId);
  }
  
  async getUserShortcuts(userId: string): Promise<MessageShortcut[]> {
    return this.store.getByUser(userId);
  }
  
  async updateShortcut(shortcutId: string, updates: Partial<MessageShortcut>): Promise<MessageShortcut | null> {
    const updated = await this.store.update(shortcutId, updates);
    
    if (updated) {
      this.eventBus.publish({
        type: SHORTCUT_EVENTS.SHORTCUT_UPDATED,
        payload: updated,
      });
    }
    
    return updated;
  }
  
  async deleteShortcut(shortcutId: string): Promise<boolean> {
    const shortcut = await this.store.get(shortcutId);
    const deleted = await this.store.delete(shortcutId);
    
    if (deleted && shortcut) {
      this.eventBus.publish({
        type: SHORTCUT_EVENTS.SHORTCUT_DELETED,
        payload: { shortcutId, userId: shortcut.userId },
      });
    }
    
    return deleted;
  }
  
  async expandText(userId: string, text: string): Promise<string> {
    // Look for shortcut triggers in text (prefixed with /)
    const shortcuts = await this.store.getByUser(userId);
    let result = text;
    
    for (const shortcut of shortcuts) {
      const triggerPattern = new RegExp(`\\/${shortcut.trigger}(?:\\s|$)`, 'gi');
      if (triggerPattern.test(result)) {
        result = result.replace(triggerPattern, shortcut.expansion + ' ');
        await this.store.incrementUsage(shortcut.id);
        
        this.eventBus.publish({
          type: SHORTCUT_EVENTS.SHORTCUT_USED,
          payload: { shortcutId: shortcut.id, userId, trigger: shortcut.trigger },
        });
      }
    }
    
    return result.trim();
  }
  
  async findMatchingShortcut(userId: string, trigger: string): Promise<MessageShortcut | null> {
    return this.store.getByTrigger(userId, trigger);
  }
  
  async searchShortcuts(userId: string, query: string): Promise<MessageShortcut[]> {
    return this.store.search(userId, query);
  }
}

// ============================================================================
// DEFAULT SHORTCUTS
// ============================================================================

export const DEFAULT_SHORTCUTS = [
  { trigger: 'ty', expansion: 'Thank you!', name: 'Thank You', category: 'Common' },
  { trigger: 'np', expansion: 'No problem!', name: 'No Problem', category: 'Common' },
  { trigger: 'omw', expansion: 'On my way!', name: 'On My Way', category: 'Common' },
  { trigger: 'brb', expansion: 'Be right back', name: 'Be Right Back', category: 'Common' },
  { trigger: 'ttyl', expansion: 'Talk to you later!', name: 'Talk Later', category: 'Common' },
  { trigger: 'idk', expansion: "I don't know", name: "Don't Know", category: 'Common' },
  { trigger: 'lmk', expansion: 'Let me know', name: 'Let Me Know', category: 'Common' },
  { trigger: 'asap', expansion: 'As soon as possible', name: 'ASAP', category: 'Work' },
  { trigger: 'fyi', expansion: 'For your information', name: 'FYI', category: 'Work' },
  { trigger: 'eod', expansion: 'End of day', name: 'End of Day', category: 'Work' },
];

// ============================================================================
// SINGLETON
// ============================================================================

let shortcutService: ShortcutService | null = null;

export function getShortcutService(): ShortcutService {
  if (!shortcutService) {
    shortcutService = new ShortcutServiceImpl(new MemoryShortcutStore(), getEventBus());
  }
  return shortcutService;
}

// Initialize default shortcuts for a user
export async function initializeDefaultShortcuts(userId: string): Promise<void> {
  const service = getShortcutService();
  
  for (const shortcut of DEFAULT_SHORTCUTS) {
    try {
      await service.createShortcut(userId, shortcut.trigger, shortcut.expansion, {
        name: shortcut.name,
        category: shortcut.category,
        isGlobal: false,
      });
    } catch {
      // Shortcut already exists, skip
    }
  }
}
