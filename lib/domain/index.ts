// Domain Services Index
// Central export for all domain services

// Core messaging features
export { getReactionService, type ReactionService } from './reactions/service';
export { getPresenceService, type PresenceService } from './presence/service';
export { getReadReceiptService, type ReadReceiptService } from './receipts/service';
export { getThreadService, type ThreadService } from './threads/service';

// Group management
export { getGroupService, type GroupService } from './groups/service';
export { getPollService, type PollService } from './polls/service';

// Media handling
export { getMediaService, type MediaService } from './media/service';

// User engagement
export { getStickerGifService, type StickerGifService } from './stickers/service';
export { getReminderService, type ReminderService } from './reminders/service';
export { getShortcutService, type ShortcutService, initializeDefaultShortcuts } from './shortcuts/service';

// AI features
export { getAIService, type AIService, createAIService, type AIProvider } from './ai/service';
export { getTranslationService, type TranslationService, createTranslationService, type TranslationProvider } from './translation/service';

// Collaboration
export { getCollaborationService, type CollaborationService } from './collaboration/service';

// Search
export { getAdvancedSearchService, type AdvancedSearchService } from './search/service';

// Events
export { getEventBus, type EventBus, type Event, type EventHandler } from './events/bus';

// Re-export event types
export { REACTION_EVENTS } from './reactions/service';
export { PRESENCE_EVENTS } from './presence/service';
export { RECEIPT_EVENTS } from './receipts/service';
export { THREAD_EVENTS } from './threads/service';
export { GROUP_EVENTS } from './groups/service';
export { POLL_EVENTS } from './polls/service';
export { MEDIA_EVENTS } from './media/service';
export { STICKER_EVENTS, GIF_EVENTS } from './stickers/service';
export { REMINDER_EVENTS } from './reminders/service';
export { SHORTCUT_EVENTS } from './shortcuts/service';
export { TRANSLATION_EVENTS } from './translation/service';
export { COLLAB_EVENTS } from './collaboration/service';
export { SEARCH_EVENTS } from './search/service';
