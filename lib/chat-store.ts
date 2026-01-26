import type { Chat, Message, User } from './types';
import { getCurrentUser } from './user-store';

// Helper function to generate consistent chat ID between two users
function getConsistentChatId(userId1: string, userId2: string): string {
  return [userId1, userId2].sort().join('_');
}

// Keys for localStorage
const CHATS_KEY = 'whatsapp_chats';
const MESSAGES_KEY = 'whatsapp_messages';

// Get all chats for the current user
export function getUserChats(): Chat[] {
  if (typeof window === 'undefined') return [];
  
  const currentUser = getCurrentUser();
  if (!currentUser) return [];
  
  try {
    const stored = localStorage.getItem(`${CHATS_KEY}_${currentUser.id}`);
    if (!stored) return [];
    const chats = JSON.parse(stored) as Chat[];
    // Convert date strings back to Date objects
    return chats.map(chat => ({
      ...chat,
      updatedAt: new Date(chat.updatedAt),
      lastMessage: chat.lastMessage ? {
        ...chat.lastMessage,
        timestamp: new Date(chat.lastMessage.timestamp),
        expiresAt: chat.lastMessage.expiresAt ? new Date(chat.lastMessage.expiresAt) : undefined,
      } : undefined,
    }));
  } catch {
    return [];
  }
}

// Save chats for the current user
export function saveUserChats(chats: Chat[]): void {
  if (typeof window === 'undefined') return;
  
  const currentUser = getCurrentUser();
  if (!currentUser) return;
  
  localStorage.setItem(`${CHATS_KEY}_${currentUser.id}`, JSON.stringify(chats));
}

// Get messages for a specific chat
export function getChatMessages(chatId: string): Message[] {
  if (typeof window === 'undefined') return [];
  
  try {
    const stored = localStorage.getItem(`${MESSAGES_KEY}_${chatId}`);
    if (!stored) return [];
    const messages = JSON.parse(stored) as Message[];
    // Convert date strings back to Date objects
    return messages.map(msg => ({
      ...msg,
      timestamp: new Date(msg.timestamp),
      expiresAt: msg.expiresAt ? new Date(msg.expiresAt) : undefined,
    }));
  } catch {
    return [];
  }
}

// Save messages for a specific chat
export function saveChatMessages(chatId: string, messages: Message[]): void {
  if (typeof window === 'undefined') return;
  localStorage.setItem(`${MESSAGES_KEY}_${chatId}`, JSON.stringify(messages));
}

// Add a message to a chat
export function addMessageToChat(chatId: string, message: Message): void {
  const messages = getChatMessages(chatId);
  
  // Check for duplicates
  if (messages.some(m => m.id === message.id)) {
    return;
  }
  
  messages.push(message);
  saveChatMessages(chatId, messages);
}

// Create or get a chat between two users
export function getOrCreateChat(otherUser: User): Chat {
  const currentUser = getCurrentUser();
  if (!currentUser) throw new Error('User not logged in');
  
  const chats = getUserChats();
  
  // Check if chat already exists
  const existingChat = chats.find(
    chat => chat.type === 'individual' && 
    chat.participants.some(p => p.id === otherUser.id)
  );
  
  if (existingChat) {
    return existingChat;
  }
  
  // Create new chat with consistent ID
  const consistentChatId = getConsistentChatId(currentUser.id, otherUser.id);
  const newChat: Chat = {
    id: consistentChatId,
    type: 'individual',
    participants: [otherUser],
    unreadCount: 0,
    isPinned: false,
    isMuted: false,
    updatedAt: new Date(),
  };
  
  chats.push(newChat);
  saveUserChats(chats);
  
  // Also save to the other user's chat list
  saveOtherUserChat(otherUser.id, currentUser, newChat.id);
  
  return newChat;
}

// Save a chat reference to another user's chat list
function saveOtherUserChat(otherUserId: string, currentUser: User, chatId: string): void {
  if (typeof window === 'undefined') return;
  
  try {
    const stored = localStorage.getItem(`${CHATS_KEY}_${otherUserId}`);
    const otherUserChats: Chat[] = stored ? JSON.parse(stored) : [];
    
    // Check if chat already exists in their list
    if (otherUserChats.some(c => c.id === chatId)) {
      return;
    }
    
    // Add chat with current user as participant
    const chatForOther: Chat = {
      id: chatId,
      type: 'individual',
      participants: [currentUser],
      unreadCount: 0,
      isPinned: false,
      isMuted: false,
      updatedAt: new Date(),
    };
    
    otherUserChats.push(chatForOther);
    localStorage.setItem(`${CHATS_KEY}_${otherUserId}`, JSON.stringify(otherUserChats));
  } catch {
    // Ignore errors
  }
}

// Update chat with last message
export function updateChatLastMessage(chatId: string, message: Message, incrementUnread: boolean = false): void {
  const currentUser = getCurrentUser();
  if (!currentUser) return;
  
  // Update current user's chat
  const chats = getUserChats();
  const chatIndex = chats.findIndex(c => c.id === chatId);
  
  if (chatIndex !== -1) {
    chats[chatIndex] = {
      ...chats[chatIndex],
      lastMessage: message,
      updatedAt: new Date(),
      unreadCount: incrementUnread && message.senderId !== currentUser.id 
        ? chats[chatIndex].unreadCount + 1 
        : chats[chatIndex].unreadCount,
    };
    saveUserChats(chats);
  }
}

// Mark chat as read
export function markChatAsRead(chatId: string): void {
  const chats = getUserChats();
  const chatIndex = chats.findIndex(c => c.id === chatId);
  
  if (chatIndex !== -1) {
    chats[chatIndex] = {
      ...chats[chatIndex],
      unreadCount: 0,
    };
    saveUserChats(chats);
  }
}

// Get chat ID for two users (deterministic)
export function getChatIdForUsers(userId1: string, userId2: string): string {
  // Use consistent format matching getConsistentChatId
  return getConsistentChatId(userId1, userId2);
}
