// Real-time chat store using Supabase when configured, localStorage fallback

import { isSupabaseConfigured } from './supabase/client';
import * as supabaseMessages from './supabase/messages';
import type { Chat, Message, User } from './types';
import { getCurrentUser } from './auth-store';
import type { RealtimeChannel } from '@supabase/supabase-js';

// Local storage keys
const CHATS_KEY = 'whatsapp_chats';
const MESSAGES_KEY = 'whatsapp_messages';

// Active subscriptions
let messageSubscription: RealtimeChannel | null = null;
let chatSubscription: RealtimeChannel | null = null;

// Message listeners for real-time updates
type MessageCallback = (message: Message) => void;
const messageListeners = new Set<MessageCallback>();

// ==================== Type Conversions ====================

function supabaseToAppMessage(sm: supabaseMessages.Message): Message {
  return {
    id: sm.id,
    content: sm.content,
    senderId: sm.senderId,
    senderName: '', // Will be populated by caller
    timestamp: new Date(sm.createdAt),
    status: sm.status,
    type: sm.type,
  };
}

// ==================== Supabase Methods ====================

async function sendMessageToSupabase(
  recipientId: string,
  content: string,
  type: Message['type'] = 'text'
): Promise<{ message: Message | null; error: string | null }> {
  const currentUser = getCurrentUser();
  if (!currentUser) {
    return { message: null, error: 'Not logged in' };
  }

  const result = await supabaseMessages.sendMessage(
    currentUser.id,
    recipientId,
    content,
    type
  );

  if (result.error || !result.message) {
    return { message: null, error: result.error || 'Failed to send message' };
  }

  const appMessage = supabaseToAppMessage(result.message);
  appMessage.senderName = currentUser.name;

  return { message: appMessage, error: null };
}

async function getMessagesFromSupabase(
  recipientId: string
): Promise<Message[]> {
  const currentUser = getCurrentUser();
  if (!currentUser) return [];

  const messages = await supabaseMessages.getMessages(currentUser.id, recipientId);
  return messages.map(supabaseToAppMessage);
}

async function getChatsFromSupabase(): Promise<{ recipientId: string; lastMessage: Message }[]> {
  const currentUser = getCurrentUser();
  if (!currentUser) return [];

  const chats = await supabaseMessages.getUserChats(currentUser.id);
  return chats.map(c => ({
    recipientId: c.recipientId,
    lastMessage: supabaseToAppMessage(c.lastMessage),
  }));
}

// ==================== LocalStorage Methods ====================

function getLocalChats(): Chat[] {
  if (typeof window === 'undefined') return [];
  const currentUser = getCurrentUser();
  if (!currentUser) return [];

  try {
    const stored = localStorage.getItem(`${CHATS_KEY}_${currentUser.id}`);
    if (!stored) return [];
    const chats = JSON.parse(stored) as Chat[];
    return chats.map(chat => ({
      ...chat,
      updatedAt: new Date(chat.updatedAt),
      lastMessage: chat.lastMessage ? {
        ...chat.lastMessage,
        timestamp: new Date(chat.lastMessage.timestamp),
      } : undefined,
    }));
  } catch {
    return [];
  }
}

function saveLocalChats(chats: Chat[]): void {
  if (typeof window === 'undefined') return;
  const currentUser = getCurrentUser();
  if (!currentUser) return;
  localStorage.setItem(`${CHATS_KEY}_${currentUser.id}`, JSON.stringify(chats));
}

function getLocalMessages(chatId: string): Message[] {
  if (typeof window === 'undefined') return [];

  try {
    const stored = localStorage.getItem(`${MESSAGES_KEY}_${chatId}`);
    if (!stored) return [];
    const messages = JSON.parse(stored) as Message[];
    return messages.map(msg => ({
      ...msg,
      timestamp: new Date(msg.timestamp),
    }));
  } catch {
    return [];
  }
}

function saveLocalMessages(chatId: string, messages: Message[]): void {
  if (typeof window === 'undefined') return;
  localStorage.setItem(`${MESSAGES_KEY}_${chatId}`, JSON.stringify(messages));
}

// ==================== Real-time Subscriptions ====================

export function subscribeToMessages(onMessage: MessageCallback): () => void {
  const currentUser = getCurrentUser();
  if (!currentUser) return () => {};

  messageListeners.add(onMessage);

  // Set up Supabase subscription if configured
  if (isSupabaseConfigured() && !messageSubscription) {
    messageSubscription = supabaseMessages.subscribeToMessages(
      currentUser.id,
      (sm) => {
        const appMessage = supabaseToAppMessage(sm);
        messageListeners.forEach(listener => listener(appMessage));
      }
    );
  }

  return () => {
    messageListeners.delete(onMessage);
    if (messageListeners.size === 0 && messageSubscription) {
      supabaseMessages.unsubscribe(messageSubscription);
      messageSubscription = null;
    }
  };
}

export function subscribeToChatMessages(
  recipientId: string,
  onMessage: MessageCallback
): () => void {
  const currentUser = getCurrentUser();
  if (!currentUser) return () => {};

  if (isSupabaseConfigured()) {
    const chatId = supabaseMessages.getChatId(currentUser.id, recipientId);
    const channel = supabaseMessages.subscribeToChat(chatId, (sm) => {
      // Only notify for messages not sent by current user
      if (sm.senderId !== currentUser.id) {
        const appMessage = supabaseToAppMessage(sm);
        onMessage(appMessage);
      }
    });

    return () => {
      if (channel) {
        supabaseMessages.unsubscribe(channel);
      }
    };
  }

  // For localStorage, we use BroadcastChannel (handled in live-chat.ts)
  return () => {};
}

// ==================== Public API ====================

// Get chat ID for two users
export function getChatId(userId1: string, userId2: string): string {
  return [userId1, userId2].sort().join('_');
}

// Send a message
export async function sendMessage(
  recipientId: string,
  content: string,
  type: Message['type'] = 'text'
): Promise<{ message: Message | null; error: string | null }> {
  const currentUser = getCurrentUser();
  if (!currentUser) {
    return { message: null, error: 'Not logged in' };
  }

  if (isSupabaseConfigured()) {
    return sendMessageToSupabase(recipientId, content, type);
  }

  // Local storage fallback
  const chatId = getChatId(currentUser.id, recipientId);
  const message: Message = {
    id: `msg-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`,
    content,
    senderId: currentUser.id,
    senderName: currentUser.name,
    timestamp: new Date(),
    status: 'sent',
    type,
  };

  // Save to localStorage
  const messages = getLocalMessages(chatId);
  messages.push(message);
  saveLocalMessages(chatId, messages);

  // Broadcast to other tabs
  broadcastMessage(chatId, message, recipientId);

  return { message, error: null };
}

// Get messages for a chat
export async function getMessages(recipientId: string): Promise<Message[]> {
  const currentUser = getCurrentUser();
  if (!currentUser) return [];

  if (isSupabaseConfigured()) {
    return getMessagesFromSupabase(recipientId);
  }

  const chatId = getChatId(currentUser.id, recipientId);
  return getLocalMessages(chatId);
}

// Get all chats for current user
export async function getUserChats(): Promise<Chat[]> {
  const currentUser = getCurrentUser();
  if (!currentUser) return [];

  if (isSupabaseConfigured()) {
    // For Supabase, we'll need to join with users table
    // For now, return from local cache or build from messages
    const chatData = await getChatsFromSupabase();
    // This would need user data - simplified for now
    return [];
  }

  return getLocalChats();
}

// Save chats (for localStorage fallback)
export function saveChats(chats: Chat[]): void {
  if (!isSupabaseConfigured()) {
    saveLocalChats(chats);
  }
}

// Mark messages as read
export async function markAsRead(recipientId: string): Promise<void> {
  const currentUser = getCurrentUser();
  if (!currentUser) return;

  if (isSupabaseConfigured()) {
    const chatId = supabaseMessages.getChatId(currentUser.id, recipientId);
    await supabaseMessages.markMessagesAsRead(chatId, currentUser.id);
  }
}

// ==================== BroadcastChannel for localStorage mode ====================

let broadcastChannel: BroadcastChannel | null = null;

function getBroadcastChannel(): BroadcastChannel | null {
  if (typeof window === 'undefined') return null;
  
  if (!broadcastChannel) {
    try {
      broadcastChannel = new BroadcastChannel('whatsapp_realtime_chat');
    } catch {
      return null;
    }
  }
  return broadcastChannel;
}

function broadcastMessage(chatId: string, message: Message, recipientId: string): void {
  const channel = getBroadcastChannel();
  if (channel) {
    channel.postMessage({
      type: 'new_message',
      chatId,
      message,
      recipientId,
    });
  }
}

// Initialize broadcast listener
export function initBroadcastListener(onMessage: (chatId: string, message: Message) => void): () => void {
  const channel = getBroadcastChannel();
  if (!channel) return () => {};

  const currentUser = getCurrentUser();
  
  const handler = (event: MessageEvent) => {
    if (event.data.type === 'new_message' && currentUser) {
      // Check if this message is for the current user
      if (event.data.recipientId === currentUser.id) {
        onMessage(event.data.chatId, event.data.message);
      }
    }
  };

  channel.addEventListener('message', handler);
  return () => channel.removeEventListener('message', handler);
}
