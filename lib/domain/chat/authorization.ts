import type { AuthContext } from '@/lib/platform/auth';
import { forbidden } from '@/lib/platform/auth';

// Chat membership and authorization service.
// In production, this queries a database of chat memberships.

export interface ChatMember {
  chatId: string;
  userId: string;
  role: 'owner' | 'admin' | 'member';
  joinedAt: string;
  isMuted: boolean;
}

export interface ChatAuthorizationService {
  // Check if user is a member of the chat
  isMember(chatId: string, userId: string): Promise<boolean>;
  
  // Get user's membership details
  getMembership(chatId: string, userId: string): Promise<ChatMember | null>;
  
  // Check if user has blocked another user
  isBlocked(blockerId: string, blockedId: string): Promise<boolean>;
  
  // Check if user can send messages to chat
  canSendMessage(chatId: string, userId: string): Promise<boolean>;
  
  // Check if user can read messages from chat
  canReadMessages(chatId: string, userId: string): Promise<boolean>;
}

// Demo implementation - allows all access for development.
// In production, implement proper membership checks.
export const demoChatAuthService: ChatAuthorizationService = {
  async isMember(chatId: string, userId: string): Promise<boolean> {
    // Demo: everyone is a member of every chat
    return true;
  },

  async getMembership(chatId: string, userId: string): Promise<ChatMember | null> {
    // Demo: return mock membership
    return {
      chatId,
      userId,
      role: 'member',
      joinedAt: new Date().toISOString(),
      isMuted: false,
    };
  },

  async isBlocked(blockerId: string, blockedId: string): Promise<boolean> {
    // Demo: no one is blocked
    return false;
  },

  async canSendMessage(chatId: string, userId: string): Promise<boolean> {
    const member = await this.getMembership(chatId, userId);
    return member !== null;
  },

  async canReadMessages(chatId: string, userId: string): Promise<boolean> {
    const member = await this.getMembership(chatId, userId);
    return member !== null;
  },
};

/**
 * Authorize a message send operation.
 * Throws if user is not allowed to send messages to this chat.
 */
export async function authorizeSendMessage(
  authService: ChatAuthorizationService,
  auth: AuthContext,
  chatId: string,
  recipientIds?: string[]
): Promise<void> {
  if (!auth.isAuthenticated || !auth.user) {
    throw forbidden('Authentication required to send messages');
  }

  const userId = auth.user.sub;

  // Check chat membership
  const canSend = await authService.canSendMessage(chatId, userId);
  if (!canSend) {
    throw forbidden('You are not a member of this chat');
  }

  // Check if sender is blocked by any recipient (for DMs)
  if (recipientIds) {
    for (const recipientId of recipientIds) {
      const isBlocked = await authService.isBlocked(recipientId, userId);
      if (isBlocked) {
        throw forbidden('You cannot send messages to this user');
      }
    }
  }
}

/**
 * Authorize reading messages from a chat.
 * Throws if user is not allowed to read this chat.
 */
export async function authorizeReadMessages(
  authService: ChatAuthorizationService,
  auth: AuthContext,
  chatId: string
): Promise<void> {
  if (!auth.isAuthenticated || !auth.user) {
    throw forbidden('Authentication required to read messages');
  }

  const userId = auth.user.sub;

  const canRead = await authService.canReadMessages(chatId, userId);
  if (!canRead) {
    throw forbidden('You are not a member of this chat');
  }
}
