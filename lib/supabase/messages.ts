import { supabase, isSupabaseConfigured } from './client';
import type { RealtimeChannel } from '@supabase/supabase-js';

export interface Message {
  id: string;
  chatId: string;
  senderId: string;
  recipientId: string;
  content: string;
  type: 'text' | 'image' | 'video' | 'audio' | 'document';
  status: 'sent' | 'delivered' | 'read';
  createdAt: string;
}

interface DbMessage {
  id: string;
  chat_id: string;
  sender_id: string;
  recipient_id: string;
  content: string;
  type: string;
  status: string;
  created_at: string;
}

function dbToMessage(dbMsg: DbMessage): Message {
  return {
    id: dbMsg.id,
    chatId: dbMsg.chat_id,
    senderId: dbMsg.sender_id,
    recipientId: dbMsg.recipient_id,
    content: dbMsg.content,
    type: dbMsg.type as Message['type'],
    status: dbMsg.status as Message['status'],
    createdAt: dbMsg.created_at,
  };
}

// Generate a consistent chat ID for two users (sorted to ensure consistency)
export function getChatId(userId1: string, userId2: string): string {
  return [userId1, userId2].sort().join('_');
}

// Send a message
export async function sendMessage(
  senderId: string,
  recipientId: string,
  content: string,
  type: Message['type'] = 'text'
): Promise<{ message: Message | null; error: string | null }> {
  if (!isSupabaseConfigured() || !supabase) {
    return { message: null, error: 'Database not configured' };
  }

  try {
    const chatId = getChatId(senderId, recipientId);

    const { data, error } = await supabase
      .from('messages')
      .insert({
        chat_id: chatId,
        sender_id: senderId,
        recipient_id: recipientId,
        content,
        type,
        status: 'sent',
      })
      .select()
      .single();

    if (error) {
      console.error('Send message error:', error);
      return { message: null, error: error.message };
    }

    return { message: dbToMessage(data), error: null };
  } catch (err) {
    console.error('Send message error:', err);
    return { message: null, error: 'Failed to send message' };
  }
}

// Get messages for a chat
export async function getMessages(
  userId1: string,
  userId2: string,
  limit: number = 50
): Promise<Message[]> {
  if (!isSupabaseConfigured() || !supabase) return [];

  try {
    const chatId = getChatId(userId1, userId2);

    const { data, error } = await supabase
      .from('messages')
      .select('*')
      .eq('chat_id', chatId)
      .order('created_at', { ascending: true })
      .limit(limit);

    if (error || !data) return [];
    return data.map(dbToMessage);
  } catch {
    return [];
  }
}

// Get all chats for a user (unique conversations)
export async function getUserChats(userId: string): Promise<{ recipientId: string; lastMessage: Message }[]> {
  if (!isSupabaseConfigured() || !supabase) return [];

  try {
    // Get all messages where user is sender or recipient
    const { data, error } = await supabase
      .from('messages')
      .select('*')
      .or(`sender_id.eq.${userId},recipient_id.eq.${userId}`)
      .order('created_at', { ascending: false });

    if (error || !data) return [];

    // Group by chat and get last message
    const chatMap = new Map<string, Message>();
    for (const msg of data) {
      const message = dbToMessage(msg);
      if (!chatMap.has(message.chatId)) {
        chatMap.set(message.chatId, message);
      }
    }

    // Convert to array with recipient info
    return Array.from(chatMap.values()).map((lastMessage) => ({
      recipientId: lastMessage.senderId === userId ? lastMessage.recipientId : lastMessage.senderId,
      lastMessage,
    }));
  } catch {
    return [];
  }
}

// Update message status
export async function updateMessageStatus(
  messageId: string,
  status: Message['status']
): Promise<void> {
  if (!isSupabaseConfigured() || !supabase) return;

  try {
    await supabase
      .from('messages')
      .update({ status })
      .eq('id', messageId);
  } catch {
    // Silent fail for status updates
  }
}

// Mark all messages as read
export async function markMessagesAsRead(
  chatId: string,
  recipientId: string
): Promise<void> {
  if (!isSupabaseConfigured() || !supabase) return;

  try {
    await supabase
      .from('messages')
      .update({ status: 'read' })
      .eq('chat_id', chatId)
      .eq('recipient_id', recipientId)
      .neq('status', 'read');
  } catch {
    // Silent fail
  }
}

// Subscribe to new messages for a user (real-time)
export function subscribeToMessages(
  userId: string,
  onMessage: (message: Message) => void
): RealtimeChannel | null {
  if (!isSupabaseConfigured() || !supabase) return null;

  const channel = supabase
    .channel(`messages:${userId}`)
    .on(
      'postgres_changes',
      {
        event: 'INSERT',
        schema: 'public',
        table: 'messages',
        filter: `recipient_id=eq.${userId}`,
      },
      (payload) => {
        const message = dbToMessage(payload.new as DbMessage);
        onMessage(message);
      }
    )
    .subscribe();

  return channel;
}

// Subscribe to messages in a specific chat (real-time)
export function subscribeToChat(
  chatId: string,
  onMessage: (message: Message) => void
): RealtimeChannel | null {
  if (!isSupabaseConfigured() || !supabase) return null;

  const channel = supabase
    .channel(`chat:${chatId}`)
    .on(
      'postgres_changes',
      {
        event: 'INSERT',
        schema: 'public',
        table: 'messages',
        filter: `chat_id=eq.${chatId}`,
      },
      (payload) => {
        const message = dbToMessage(payload.new as DbMessage);
        onMessage(message);
      }
    )
    .subscribe();

  return channel;
}

// Unsubscribe from a channel
export function unsubscribe(channel: RealtimeChannel | null): void {
  if (!channel || !supabase) return;
  supabase.removeChannel(channel);
}

// Clear all messages in a chat (for both users)
export async function clearChat(
  userId1: string,
  userId2: string
): Promise<{ success: boolean; error: string | null }> {
  if (!isSupabaseConfigured() || !supabase) {
    return { success: false, error: 'Database not configured' };
  }

  try {
    const chatId = getChatId(userId1, userId2);

    const { error } = await supabase
      .from('messages')
      .delete()
      .eq('chat_id', chatId);

    if (error) {
      console.error('Clear chat error:', error);
      return { success: false, error: error.message };
    }

    return { success: true, error: null };
  } catch (err) {
    console.error('Clear chat error:', err);
    return { success: false, error: 'Failed to clear chat' };
  }
}

// Delete all messages for a user (when deleting account)
export async function deleteUserMessages(
  userId: string
): Promise<{ success: boolean; error: string | null }> {
  if (!isSupabaseConfigured() || !supabase) {
    return { success: false, error: 'Database not configured' };
  }

  try {
    const { error } = await supabase
      .from('messages')
      .delete()
      .or(`sender_id.eq.${userId},recipient_id.eq.${userId}`);

    if (error) {
      console.error('Delete user messages error:', error);
      return { success: false, error: error.message };
    }

    return { success: true, error: null };
  } catch (err) {
    console.error('Delete user messages error:', err);
    return { success: false, error: 'Failed to delete messages' };
  }
}
