// AI Features Service
// Handles smart replies, conversation starters, and summaries

import type {
  ConversationStarterRequest,
  ConversationStarterResponse,
  ConversationStarter,
  SmartReplyRequest,
  SmartReplyResponse,
  SmartReply,
  ConversationSummaryRequest,
  ConversationSummary,
  ActionItem,
  KeyTopic,
  AIUserPreferences,
} from '@/lib/contracts/ai-features';

// ============================================================================
// AI PROVIDER INTERFACE
// ============================================================================

export interface AIProvider {
  generateConversationStarters(request: ConversationStarterRequest): Promise<ConversationStarter[]>;
  generateSmartReplies(request: SmartReplyRequest): Promise<SmartReply[]>;
  generateConversationSummary(messages: MessageForSummary[], options: SummaryOptions): Promise<ConversationSummaryResult>;
  moderateContent(content: string): Promise<ModerationResult>;
}

export interface MessageForSummary {
  senderId: string;
  senderName?: string;
  content: string;
  timestamp: string;
  type: string;
}

export interface SummaryOptions {
  maxLength: 'brief' | 'moderate' | 'detailed';
  includeActionItems: boolean;
  includeKeyTopics: boolean;
  language?: string;
}

export interface ConversationSummaryResult {
  summary: string;
  actionItems?: ActionItem[];
  keyTopics?: KeyTopic[];
  decisions?: string[];
}

export interface ModerationResult {
  isFlagged: boolean;
  categories: Record<string, number>;
  action: 'allow' | 'warn' | 'block';
}

// ============================================================================
// DEMO AI PROVIDER
// ============================================================================

export class DemoAIProvider implements AIProvider {
  async generateConversationStarters(request: ConversationStarterRequest): Promise<ConversationStarter[]> {
    // Demo responses
    const starters: ConversationStarter[] = [
      {
        id: 'cs-1',
        text: 'Hey! How have you been?',
        category: 'greeting',
        confidence: 0.9,
        tone: 'friendly',
      },
      {
        id: 'cs-2',
        text: "What's new with you?",
        category: 'question',
        confidence: 0.85,
        tone: 'casual',
      },
      {
        id: 'cs-3',
        text: 'I was just thinking about you!',
        category: 'greeting',
        confidence: 0.8,
        tone: 'friendly',
      },
    ];
    
    return starters.slice(0, request.count ?? 3);
  }
  
  async generateSmartReplies(request: SmartReplyRequest): Promise<SmartReply[]> {
    const content = request.messageContent.toLowerCase();
    
    // Simple rule-based responses for demo
    const replies: SmartReply[] = [];
    
    if (content.includes('?')) {
      replies.push({
        id: 'sr-1',
        text: 'Yes, sure!',
        type: 'affirmative',
        confidence: 0.85,
        isEmoji: false,
      });
      replies.push({
        id: 'sr-2',
        text: "Let me check and get back to you",
        type: 'continuation',
        confidence: 0.75,
        isEmoji: false,
      });
    }
    
    if (content.includes('thanks') || content.includes('thank you')) {
      replies.push({
        id: 'sr-3',
        text: "You're welcome! 😊",
        type: 'acknowledgment',
        confidence: 0.9,
        isEmoji: false,
      });
    }
    
    // Default replies
    if (replies.length === 0) {
      replies.push(
        { id: 'sr-4', text: '👍', type: 'emoji', confidence: 0.7, isEmoji: true },
        { id: 'sr-5', text: 'Got it!', type: 'acknowledgment', confidence: 0.8, isEmoji: false },
        { id: 'sr-6', text: 'Sounds good!', type: 'affirmative', confidence: 0.75, isEmoji: false },
      );
    }
    
    return replies.slice(0, request.count ?? 3);
  }
  
  async generateConversationSummary(
    messages: MessageForSummary[],
    options: SummaryOptions
  ): Promise<ConversationSummaryResult> {
    const messageCount = messages.length;
    
    // Demo summary
    const summary = `This conversation contains ${messageCount} messages. ` +
      `The participants discussed various topics. ` +
      `Key points and action items are highlighted below.`;
    
    const result: ConversationSummaryResult = {
      summary,
    };
    
    if (options.includeActionItems) {
      result.actionItems = [
        {
          id: 'ai-1',
          text: 'Follow up on discussed items',
          priority: 'medium',
        },
      ];
    }
    
    if (options.includeKeyTopics) {
      result.keyTopics = [
        {
          topic: 'General Discussion',
          messageCount: messageCount,
          sentiment: 'neutral',
          keyPoints: ['Various topics were covered'],
        },
      ];
    }
    
    return result;
  }
  
  async moderateContent(content: string): Promise<ModerationResult> {
    // Demo moderation - always pass
    return {
      isFlagged: false,
      categories: {
        hate: 0,
        harassment: 0,
        violence: 0,
        sexual: 0,
        spam: 0,
      },
      action: 'allow',
    };
  }
}

// ============================================================================
// SERVICE
// ============================================================================

export interface AIService {
  getConversationStarters(request: ConversationStarterRequest): Promise<ConversationStarterResponse>;
  getSmartReplies(request: SmartReplyRequest): Promise<SmartReplyResponse>;
  getConversationSummary(request: ConversationSummaryRequest, messages: MessageForSummary[]): Promise<ConversationSummary>;
  getUserPreferences(userId: string): Promise<AIUserPreferences>;
  updateUserPreferences(userId: string, updates: Partial<AIUserPreferences>): Promise<AIUserPreferences>;
}

export class AIFeatureService implements AIService {
  private provider: AIProvider;
  private preferences = new Map<string, AIUserPreferences>();
  
  constructor(provider: AIProvider) {
    this.provider = provider;
  }
  
  async getConversationStarters(request: ConversationStarterRequest): Promise<ConversationStarterResponse> {
    const suggestions = await this.provider.generateConversationStarters(request);
    
    return {
      chatId: request.chatId,
      suggestions,
      generatedAt: new Date().toISOString(),
    };
  }
  
  async getSmartReplies(request: SmartReplyRequest): Promise<SmartReplyResponse> {
    const suggestions = await this.provider.generateSmartReplies(request);
    
    return {
      messageId: request.messageId,
      suggestions,
      generatedAt: new Date().toISOString(),
      expiresAt: new Date(Date.now() + 300000).toISOString(), // 5 minutes
    };
  }
  
  async getConversationSummary(
    request: ConversationSummaryRequest,
    messages: MessageForSummary[]
  ): Promise<ConversationSummary> {
    const result = await this.provider.generateConversationSummary(messages, {
      maxLength: request.maxLength ?? 'moderate',
      includeActionItems: request.includeActionItems ?? true,
      includeKeyTopics: request.includeKeyTopics ?? true,
      language: request.language,
    });
    
    const timestamps = messages.map(m => m.timestamp).sort();
    
    return {
      id: `summary-${Date.now()}`,
      chatId: request.chatId,
      summary: result.summary,
      actionItems: result.actionItems,
      keyTopics: result.keyTopics,
      decisions: result.decisions,
      messageCount: messages.length,
      timeRange: {
        start: timestamps[0] ?? new Date().toISOString(),
        end: timestamps[timestamps.length - 1] ?? new Date().toISOString(),
      },
      generatedAt: new Date().toISOString(),
    };
  }
  
  async getUserPreferences(userId: string): Promise<AIUserPreferences> {
    const existing = this.preferences.get(userId);
    if (existing) return existing;
    
    // Default preferences
    const defaults: AIUserPreferences = {
      userId,
      smartRepliesEnabled: true,
      conversationStartersEnabled: true,
      autoSummaryEnabled: false,
      allowContextAnalysis: true,
      excludedChats: [],
      updatedAt: new Date().toISOString(),
    };
    
    this.preferences.set(userId, defaults);
    return defaults;
  }
  
  async updateUserPreferences(
    userId: string,
    updates: Partial<AIUserPreferences>
  ): Promise<AIUserPreferences> {
    const existing = await this.getUserPreferences(userId);
    const updated: AIUserPreferences = {
      ...existing,
      ...updates,
      userId,
      updatedAt: new Date().toISOString(),
    };
    
    this.preferences.set(userId, updated);
    return updated;
  }
}

// ============================================================================
// SINGLETON
// ============================================================================

let aiService: AIService | null = null;

export function getAIService(): AIService {
  if (!aiService) {
    aiService = new AIFeatureService(new DemoAIProvider());
  }
  return aiService;
}

// For testing/production with different providers
export function createAIService(provider: AIProvider): AIService {
  return new AIFeatureService(provider);
}
