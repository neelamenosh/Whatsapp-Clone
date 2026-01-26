import type { Chat, Message, User } from './types';
import { getCurrentUser } from './user-store';

// Generate consistent chat ID between two users (sorted to ensure both see the same ID)
export function getConsistentChatId(userId1: string, userId2: string): string {
  return [userId1, userId2].sort().join('_');
}

type MessageListener = (chatId: string, message: Message) => void;
type TypingListener = (chatId: string, userId: string, isTyping: boolean) => void;
type OnlineListener = (userId: string, isOnline: boolean) => void;
type NewChatListener = (chat: Chat) => void;

interface LiveChatMessage {
  type: 'message' | 'typing' | 'online' | 'read' | 'new_chat';
  chatId?: string;
  userId?: string;
  recipientId?: string;
  message?: Message;
  chat?: Chat;
  sender?: User;
  isTyping?: boolean;
  isOnline?: boolean;
  messageIds?: string[];
}

class LiveChatService {
  private static instance: LiveChatService;
  private messageListeners = new Set<MessageListener>();
  private typingListeners = new Set<TypingListener>();
  private onlineListeners = new Set<OnlineListener>();
  private newChatListeners = new Set<NewChatListener>();
  private broadcastChannel: BroadcastChannel | null = null;
  private pollingInterval: number | null = null;
  private lastPollTime = 0;

  private constructor() {
    this.init();
  }

  static getInstance(): LiveChatService {
    if (!LiveChatService.instance) {
      LiveChatService.instance = new LiveChatService();
    }
    return LiveChatService.instance;
  }

  private init() {
    if (typeof window === 'undefined') return;

    // Use BroadcastChannel for cross-tab communication
    try {
      this.broadcastChannel = new BroadcastChannel('whatsapp_live_chat');
      this.broadcastChannel.onmessage = (event) => {
        this.handleIncomingMessage(event.data);
      };
    } catch {
      // BroadcastChannel not supported, fall back to localStorage events
      window.addEventListener('storage', (e) => {
        if (e.key === 'whatsapp_live_message' && e.newValue) {
          try {
            const data = JSON.parse(e.newValue);
            this.handleIncomingMessage(data);
          } catch {
            // Ignore parse errors
          }
        }
      });
    }

    // Start polling for messages (fallback mechanism)
    this.startPolling();

    // Set online status
    this.setOnline(true);

    // Handle page visibility changes
    document.addEventListener('visibilitychange', () => {
      this.setOnline(!document.hidden);
    });

    // Handle window close
    window.addEventListener('beforeunload', () => {
      this.setOnline(false);
    });
  }

  private startPolling() {
    if (this.pollingInterval) return;
    
    // Poll for new messages every 500ms
    this.pollingInterval = window.setInterval(() => {
      this.checkForNewMessages();
    }, 500);
  }

  private checkForNewMessages() {
    if (typeof window === 'undefined') return;
    
    try {
      const stored = localStorage.getItem('whatsapp_pending_messages');
      if (!stored) return;
      
      const messages: Array<{ timestamp: number; data: LiveChatMessage }> = JSON.parse(stored);
      const newMessages = messages.filter(m => m.timestamp > this.lastPollTime);
      
      if (newMessages.length > 0) {
        this.lastPollTime = Date.now();
        newMessages.forEach(m => this.handleIncomingMessage(m.data));
      }
    } catch {
      // Ignore errors
    }
  }

  private handleIncomingMessage(data: LiveChatMessage) {
    const currentUser = getCurrentUser();
    if (!currentUser) return;

    switch (data.type) {
      case 'message':
        if (data.message) {
          // Don't notify for our own messages
          if (data.message.senderId === currentUser.id) return;
          
          // Use the consistent chat ID or the provided chatId
          const consistentChatId = data.chatId || getConsistentChatId(currentUser.id, data.message.senderId);
          
          // Store message for this user's view
          this.storeMessageForUser(consistentChatId, data.message);
          
          this.messageListeners.forEach(listener => {
            listener(consistentChatId, data.message!);
          });
        }
        break;
      
      case 'typing':
        if (data.chatId && data.userId && data.userId !== currentUser.id) {
          this.typingListeners.forEach(listener => {
            listener(data.chatId!, data.userId!, data.isTyping ?? false);
          });
        }
        break;
      
      case 'online':
        if (data.userId && data.userId !== currentUser.id) {
          this.onlineListeners.forEach(listener => {
            listener(data.userId!, data.isOnline ?? false);
          });
        }
        break;
      
      case 'new_chat':
        // Check if this chat is for the current user
        if (data.recipientId === currentUser.id && data.chat && data.sender) {
          // Generate consistent chat ID for the recipient
          const consistentChatId = getConsistentChatId(currentUser.id, data.sender.id);
          
          // Create a chat object for the recipient with the sender as participant
          const chatForRecipient: Chat = {
            ...data.chat,
            id: consistentChatId, // Use consistent chat ID
            participants: [data.sender],
          };
          this.newChatListeners.forEach(listener => {
            listener(chatForRecipient);
          });
        }
        break;
    }
  }

  private broadcast(data: LiveChatMessage) {
    if (typeof window === 'undefined') return;

    // Use BroadcastChannel if available
    if (this.broadcastChannel) {
      this.broadcastChannel.postMessage(data);
    }

    // Also store in localStorage for cross-tab fallback
    try {
      const stored = localStorage.getItem('whatsapp_pending_messages');
      const messages: Array<{ timestamp: number; data: LiveChatMessage }> = stored ? JSON.parse(stored) : [];
      
      messages.push({ timestamp: Date.now(), data });
      
      // Keep only last 100 messages to prevent localStorage bloat
      const trimmed = messages.slice(-100);
      localStorage.setItem('whatsapp_pending_messages', JSON.stringify(trimmed));
      
      // Trigger storage event for other tabs
      localStorage.setItem('whatsapp_live_message', JSON.stringify(data));
    } catch {
      // Ignore errors
    }
  }

  // Store message for a user
  private storeMessageForUser(chatId: string, message: Message) {
    const messagesKey = `whatsapp_messages_${chatId}`;
    try {
      const stored = localStorage.getItem(messagesKey);
      const messages: Message[] = stored ? JSON.parse(stored) : [];
      
      // Check for duplicates
      if (!messages.some(m => m.id === message.id)) {
        messages.push(message);
        localStorage.setItem(messagesKey, JSON.stringify(messages));
      }
    } catch {
      // Ignore errors
    }
  }

  // Public methods
  sendMessage(chatId: string, message: Message, recipientId?: string) {
    const currentUser = getCurrentUser();
    if (!currentUser) return;
    
    // Determine the recipient ID
    const targetRecipientId = recipientId || (message.senderId === currentUser.id ? 
      chatId.replace(currentUser.id, '').replace('_', '').replace('chat-', '') : currentUser.id);
    
    // Use consistent chat ID for storage
    const consistentChatId = targetRecipientId ? 
      getConsistentChatId(currentUser.id, targetRecipientId) : chatId;
    
    // Store message locally
    this.storeMessageForUser(consistentChatId, message);

    // Broadcast to other tabs/windows with recipient info
    this.broadcast({
      type: 'message',
      chatId: consistentChatId,
      message,
      recipientId: targetRecipientId,
    });
  }

  sendTyping(chatId: string, isTyping: boolean) {
    const currentUser = getCurrentUser();
    if (!currentUser) return;

    this.broadcast({
      type: 'typing',
      chatId,
      userId: currentUser.id,
      isTyping,
    });
  }

  setOnline(isOnline: boolean) {
    const currentUser = getCurrentUser();
    if (!currentUser) return;

    // Update user's online status in localStorage
    try {
      const onlineUsers = JSON.parse(localStorage.getItem('whatsapp_online_users') || '{}');
      if (isOnline) {
        onlineUsers[currentUser.id] = Date.now();
      } else {
        delete onlineUsers[currentUser.id];
      }
      localStorage.setItem('whatsapp_online_users', JSON.stringify(onlineUsers));
    } catch {
      // Ignore errors
    }

    this.broadcast({
      type: 'online',
      userId: currentUser.id,
      isOnline,
    });
  }

  isUserOnline(userId: string): boolean {
    try {
      const onlineUsers = JSON.parse(localStorage.getItem('whatsapp_online_users') || '{}');
      const lastSeen = onlineUsers[userId];
      // Consider online if seen in last 30 seconds
      return lastSeen && (Date.now() - lastSeen) < 30000;
    } catch {
      return false;
    }
  }

  onMessage(listener: MessageListener): () => void {
    this.messageListeners.add(listener);
    return () => this.messageListeners.delete(listener);
  }

  onTyping(listener: TypingListener): () => void {
    this.typingListeners.add(listener);
    return () => this.typingListeners.delete(listener);
  }

  onOnlineStatus(listener: OnlineListener): () => void {
    this.onlineListeners.add(listener);
    return () => this.onlineListeners.delete(listener);
  }

  onNewChat(listener: NewChatListener): () => void {
    this.newChatListeners.add(listener);
    return () => this.newChatListeners.delete(listener);
  }

  // Broadcast a new chat to the recipient
  notifyNewChat(chat: Chat, recipientId: string) {
    const currentUser = getCurrentUser();
    if (!currentUser) return;

    this.broadcast({
      type: 'new_chat',
      chatId: chat.id,
      chat,
      recipientId,
      sender: currentUser,
    });
  }

  // Get messages from localStorage
  getMessages(chatId: string): Message[] {
    try {
      const stored = localStorage.getItem(`whatsapp_messages_${chatId}`);
      if (!stored) return [];
      const messages = JSON.parse(stored) as Message[];
      return messages.map(msg => ({
        ...msg,
        timestamp: new Date(msg.timestamp),
        expiresAt: msg.expiresAt ? new Date(msg.expiresAt) : undefined,
      }));
    } catch {
      return [];
    }
  }

  destroy() {
    if (this.broadcastChannel) {
      this.broadcastChannel.close();
    }
    if (this.pollingInterval) {
      window.clearInterval(this.pollingInterval);
    }
    this.setOnline(false);
  }
}

// Export singleton instance getter
export function getLiveChatService(): LiveChatService {
  return LiveChatService.getInstance();
}
