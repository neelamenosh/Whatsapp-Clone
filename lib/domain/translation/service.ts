// Message Translation Service
// Handles automatic and on-demand message translation

import type { MessageTranslation } from '@/lib/contracts/engagement';
import { getEventBus, type EventBus } from '@/lib/domain/events/bus';

// ============================================================================
// EVENTS
// ============================================================================

export const TRANSLATION_EVENTS = {
  TRANSLATION_COMPLETED: 'translation:completed',
  TRANSLATION_FAILED: 'translation:failed',
  AUTO_TRANSLATE_ENABLED: 'translation:auto_enabled',
  AUTO_TRANSLATE_DISABLED: 'translation:auto_disabled',
} as const;

export interface TranslationCompletedEvent {
  type: typeof TRANSLATION_EVENTS.TRANSLATION_COMPLETED;
  payload: MessageTranslation;
}

export interface TranslationFailedEvent {
  type: typeof TRANSLATION_EVENTS.TRANSLATION_FAILED;
  payload: {
    messageId: string;
    targetLanguage: string;
    error: string;
  };
}

// ============================================================================
// TRANSLATION PROVIDER INTERFACE
// ============================================================================

export interface TranslationProvider {
  translate(text: string, targetLanguage: string, sourceLanguage?: string): Promise<TranslationResult>;
  detectLanguage(text: string): Promise<LanguageDetectionResult>;
  getSupportedLanguages(): Promise<SupportedLanguage[]>;
}

export interface TranslationResult {
  translatedText: string;
  sourceLanguage: string;
  confidence: number;
}

export interface LanguageDetectionResult {
  language: string;
  confidence: number;
  alternatives?: Array<{ language: string; confidence: number }>;
}

export interface SupportedLanguage {
  code: string;
  name: string;
  nativeName: string;
}

// ============================================================================
// DEMO TRANSLATION PROVIDER
// ============================================================================

export class DemoTranslationProvider implements TranslationProvider {
  private languages: SupportedLanguage[] = [
    { code: 'en', name: 'English', nativeName: 'English' },
    { code: 'es', name: 'Spanish', nativeName: 'Español' },
    { code: 'fr', name: 'French', nativeName: 'Français' },
    { code: 'de', name: 'German', nativeName: 'Deutsch' },
    { code: 'it', name: 'Italian', nativeName: 'Italiano' },
    { code: 'pt', name: 'Portuguese', nativeName: 'Português' },
    { code: 'zh', name: 'Chinese', nativeName: '中文' },
    { code: 'ja', name: 'Japanese', nativeName: '日本語' },
    { code: 'ko', name: 'Korean', nativeName: '한국어' },
    { code: 'ar', name: 'Arabic', nativeName: 'العربية' },
    { code: 'hi', name: 'Hindi', nativeName: 'हिन्दी' },
    { code: 'ru', name: 'Russian', nativeName: 'Русский' },
  ];
  
  async translate(
    text: string,
    targetLanguage: string,
    sourceLanguage?: string
  ): Promise<TranslationResult> {
    // Demo: Return text with language indicator
    const source = sourceLanguage ?? 'auto';
    
    return {
      translatedText: `[${targetLanguage.toUpperCase()}] ${text}`,
      sourceLanguage: source === 'auto' ? 'en' : source,
      confidence: 0.95,
    };
  }
  
  async detectLanguage(text: string): Promise<LanguageDetectionResult> {
    // Demo: Always detect as English
    return {
      language: 'en',
      confidence: 0.9,
      alternatives: [
        { language: 'es', confidence: 0.05 },
        { language: 'fr', confidence: 0.03 },
      ],
    };
  }
  
  async getSupportedLanguages(): Promise<SupportedLanguage[]> {
    return this.languages;
  }
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface TranslationStore {
  save(translation: MessageTranslation): Promise<void>;
  get(messageId: string, targetLanguage: string): Promise<MessageTranslation | null>;
  getForMessage(messageId: string): Promise<MessageTranslation[]>;
  
  // User preferences
  setUserPreference(userId: string, targetLanguage: string): Promise<void>;
  getUserPreference(userId: string): Promise<string | null>;
  setAutoTranslate(userId: string, chatId: string, enabled: boolean): Promise<void>;
  isAutoTranslateEnabled(userId: string, chatId: string): Promise<boolean>;
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryTranslationStore implements TranslationStore {
  private translations = new Map<string, MessageTranslation>(); // key: messageId:targetLang
  private userPreferences = new Map<string, string>(); // userId -> targetLanguage
  private autoTranslate = new Map<string, boolean>(); // userId:chatId -> enabled
  
  async save(translation: MessageTranslation): Promise<void> {
    const key = `${translation.messageId}:${translation.targetLanguage}`;
    this.translations.set(key, translation);
  }
  
  async get(messageId: string, targetLanguage: string): Promise<MessageTranslation | null> {
    const key = `${messageId}:${targetLanguage}`;
    return this.translations.get(key) ?? null;
  }
  
  async getForMessage(messageId: string): Promise<MessageTranslation[]> {
    return Array.from(this.translations.values())
      .filter(t => t.messageId === messageId);
  }
  
  async setUserPreference(userId: string, targetLanguage: string): Promise<void> {
    this.userPreferences.set(userId, targetLanguage);
  }
  
  async getUserPreference(userId: string): Promise<string | null> {
    return this.userPreferences.get(userId) ?? null;
  }
  
  async setAutoTranslate(userId: string, chatId: string, enabled: boolean): Promise<void> {
    const key = `${userId}:${chatId}`;
    this.autoTranslate.set(key, enabled);
  }
  
  async isAutoTranslateEnabled(userId: string, chatId: string): Promise<boolean> {
    const key = `${userId}:${chatId}`;
    return this.autoTranslate.get(key) ?? false;
  }
}

// ============================================================================
// SERVICE
// ============================================================================

export interface TranslationService {
  translateMessage(
    messageId: string,
    originalText: string,
    targetLanguage: string,
    sourceLanguage?: string
  ): Promise<MessageTranslation>;
  
  getTranslation(messageId: string, targetLanguage: string): Promise<MessageTranslation | null>;
  detectLanguage(text: string): Promise<LanguageDetectionResult>;
  getSupportedLanguages(): Promise<SupportedLanguage[]>;
  
  // User preferences
  setUserPreferredLanguage(userId: string, language: string): Promise<void>;
  getUserPreferredLanguage(userId: string): Promise<string | null>;
  enableAutoTranslate(userId: string, chatId: string): Promise<void>;
  disableAutoTranslate(userId: string, chatId: string): Promise<void>;
  shouldAutoTranslate(userId: string, chatId: string): Promise<boolean>;
}

export class TranslationServiceImpl implements TranslationService {
  private store: TranslationStore;
  private provider: TranslationProvider;
  private eventBus: EventBus;
  
  constructor(store: TranslationStore, provider: TranslationProvider, eventBus: EventBus) {
    this.store = store;
    this.provider = provider;
    this.eventBus = eventBus;
  }
  
  async translateMessage(
    messageId: string,
    originalText: string,
    targetLanguage: string,
    sourceLanguage?: string
  ): Promise<MessageTranslation> {
    // Check cache first
    const cached = await this.store.get(messageId, targetLanguage);
    if (cached) return cached;
    
    try {
      const result = await this.provider.translate(originalText, targetLanguage, sourceLanguage);
      
      const translation: MessageTranslation = {
        messageId,
        originalText,
        translatedText: result.translatedText,
        sourceLanguage: result.sourceLanguage,
        targetLanguage,
        provider: 'demo', // Replace with actual provider name
        confidence: result.confidence,
        translatedAt: new Date().toISOString(),
      };
      
      await this.store.save(translation);
      
      this.eventBus.publish({
        type: TRANSLATION_EVENTS.TRANSLATION_COMPLETED,
        payload: translation,
      });
      
      return translation;
    } catch (error) {
      this.eventBus.publish({
        type: TRANSLATION_EVENTS.TRANSLATION_FAILED,
        payload: {
          messageId,
          targetLanguage,
          error: error instanceof Error ? error.message : 'Translation failed',
        },
      });
      throw error;
    }
  }
  
  async getTranslation(messageId: string, targetLanguage: string): Promise<MessageTranslation | null> {
    return this.store.get(messageId, targetLanguage);
  }
  
  async detectLanguage(text: string): Promise<LanguageDetectionResult> {
    return this.provider.detectLanguage(text);
  }
  
  async getSupportedLanguages(): Promise<SupportedLanguage[]> {
    return this.provider.getSupportedLanguages();
  }
  
  async setUserPreferredLanguage(userId: string, language: string): Promise<void> {
    await this.store.setUserPreference(userId, language);
  }
  
  async getUserPreferredLanguage(userId: string): Promise<string | null> {
    return this.store.getUserPreference(userId);
  }
  
  async enableAutoTranslate(userId: string, chatId: string): Promise<void> {
    await this.store.setAutoTranslate(userId, chatId, true);
    
    this.eventBus.publish({
      type: TRANSLATION_EVENTS.AUTO_TRANSLATE_ENABLED,
      payload: { userId, chatId },
    });
  }
  
  async disableAutoTranslate(userId: string, chatId: string): Promise<void> {
    await this.store.setAutoTranslate(userId, chatId, false);
    
    this.eventBus.publish({
      type: TRANSLATION_EVENTS.AUTO_TRANSLATE_DISABLED,
      payload: { userId, chatId },
    });
  }
  
  async shouldAutoTranslate(userId: string, chatId: string): Promise<boolean> {
    return this.store.isAutoTranslateEnabled(userId, chatId);
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let translationService: TranslationService | null = null;

export function getTranslationService(): TranslationService {
  if (!translationService) {
    translationService = new TranslationServiceImpl(
      new MemoryTranslationStore(),
      new DemoTranslationProvider(),
      getEventBus()
    );
  }
  return translationService;
}

// For testing/production with different providers
export function createTranslationService(
  provider: TranslationProvider,
  store?: TranslationStore
): TranslationService {
  return new TranslationServiceImpl(
    store ?? new MemoryTranslationStore(),
    provider,
    getEventBus()
  );
}
