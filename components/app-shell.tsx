'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { cn } from '@/lib/utils';
import type { TabType, Chat, User } from '@/lib/types';
import { calls, getChatById, setCurrentUserData } from '@/lib/mock-data';
import { getCurrentUser, isDatabaseConfigured, initPresenceTracking, findUserById } from '@/lib/auth-store';
import { getUserChats, saveUserChats } from '@/lib/chat-store';
import { getLiveChatService, getConsistentChatId } from '@/lib/live-chat';
import { isSupabaseConfigured } from '@/lib/supabase/client';
import * as supabaseMessages from '@/lib/supabase/messages';
import * as supabaseUsers from '@/lib/supabase/users';
import { TabBar } from './navigation/tab-bar';
import { ChatList } from './chat/chat-list';
import { ConversationView } from './chat/conversation-view';
import { StatusList } from './status/status-list';
import { CallsList } from './calls/calls-list';
import { SettingsModal } from './settings/settings-modal';

export function AppShell() {
  const router = useRouter();
  const [activeTab, setActiveTab] = useState<TabType>('chats');
  const [selectedChatId, setSelectedChatId] = useState<string | null>(null);
  const [isSettingsOpen, setIsSettingsOpen] = useState(false);
  const [chats, setChats] = useState<Chat[]>([]);
  const [isLoading, setIsLoading] = useState(true);

  // Check if user is logged in and load chats
  useEffect(() => {
    const currentUser = getCurrentUser();
    if (!currentUser) {
      router.push('/login');
    } else {
      // Set current user data in mock-data
      setCurrentUserData(currentUser);
      
      // Load user's chats from localStorage
      const userChats = getUserChats();
      setChats(userChats);
      
      // Initialize live chat service
      getLiveChatService();
      
      // Initialize presence tracking (sets user online and tracks visibility)
      const cleanupPresence = initPresenceTracking();
      
      setIsLoading(false);
      
      return () => {
        cleanupPresence();
      };
    }
  }, [router]);

  // Listen for new messages and update chat list
  useEffect(() => {
    if (isLoading) return;
    
    const liveChatService = getLiveChatService();
    const currentUser = getCurrentUser();
    
    // Helper function to update chat list with new message
    const handleNewMessage = async (chatId: string, message: any, senderId: string) => {
      const currentUserNow = getCurrentUser();
      if (!currentUserNow) return;
      
      setChats((prev) => {
        // Find chat by ID or by matching the consistent chat ID with participants
        let chatIndex = prev.findIndex(c => c.id === chatId);
        
        // If not found directly, check if any chat has a participant that matches
        if (chatIndex === -1) {
          chatIndex = prev.findIndex(c => {
            if (c.type !== 'individual') return false;
            const participant = c.participants[0];
            const expectedChatId = getConsistentChatId(currentUserNow.id, participant.id);
            return expectedChatId === chatId;
          });
        }
        
        // Also check by sender ID
        if (chatIndex === -1) {
          chatIndex = prev.findIndex(c => {
            if (c.type !== 'individual') return false;
            const participant = c.participants[0];
            return participant.id === senderId;
          });
        }
        
        if (chatIndex === -1) {
          // Chat doesn't exist yet - we need to create a new chat
          // This will be handled asynchronously below
          return prev;
        }
        
        const updated = [...prev];
        updated[chatIndex] = {
          ...updated[chatIndex],
          lastMessage: message,
          updatedAt: new Date(),
          unreadCount: selectedChatId === chatId || selectedChatId === updated[chatIndex].id
            ? updated[chatIndex].unreadCount 
            : (senderId !== currentUserNow?.id ? updated[chatIndex].unreadCount + 1 : updated[chatIndex].unreadCount),
        };
        
        // Sort by updatedAt
        updated.sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
        
        // Persist to localStorage
        saveUserChats(updated);
        
        return updated;
      });
    };
    
    // Helper to create a new chat when message comes from unknown user
    const createChatForNewMessage = async (chatId: string, message: any, senderId: string) => {
      const currentUserNow = getCurrentUser();
      if (!currentUserNow || senderId === currentUserNow.id) return;
      
      // Check if chat already exists
      const existingChat = chats.find(c => {
        if (c.id === chatId) return true;
        if (c.type !== 'individual') return false;
        const participant = c.participants[0];
        return participant.id === senderId || getConsistentChatId(currentUserNow.id, participant.id) === chatId;
      });
      
      if (existingChat) return;
      
      // Fetch sender's info from Supabase or local
      let senderUser: User | null = null;
      
      if (isSupabaseConfigured()) {
        const supaUser = await supabaseUsers.getUserById(senderId);
        if (supaUser) {
          senderUser = {
            id: supaUser.id,
            name: supaUser.displayName,
            email: supaUser.email,
            avatar: supaUser.avatar || 'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face',
            status: supaUser.status || 'online',
            about: supaUser.bio || 'Available',
            phone: supaUser.phone,
          };
        }
      }
      
      if (!senderUser) {
        // Try local lookup
        senderUser = await findUserById(senderId);
      }
      
      if (!senderUser) {
        console.log('Could not find sender user info:', senderId);
        return;
      }
      
      // Create new chat
      const newChat: Chat = {
        id: chatId,
        type: 'individual',
        participants: [senderUser],
        lastMessage: message,
        unreadCount: 1,
        isPinned: false,
        isMuted: false,
        updatedAt: new Date(),
      };
      
      setChats((prev) => {
        // Double check it doesn't exist
        if (prev.some(c => c.id === chatId || prev.some(c => c.participants[0]?.id === senderId))) {
          return prev;
        }
        const updated = [newChat, ...prev];
        saveUserChats(updated);
        return updated;
      });
    };
    
    // Subscribe to localStorage/BroadcastChannel updates
    const unsubMessage = liveChatService.onMessage((chatId, message) => {
      handleNewMessage(chatId, message, message.senderId);
      // Also try to create chat if it doesn't exist
      createChatForNewMessage(chatId, message, message.senderId);
    });

    // Subscribe to Supabase real-time messages if configured
    let supabaseUnsubscribe: (() => void) | null = null;
    if (isSupabaseConfigured() && currentUser) {
      const channel = supabaseMessages.subscribeToMessages(currentUser.id, (msg) => {
        const formattedMessage = {
          id: msg.id,
          senderId: msg.senderId,
          content: msg.content,
          timestamp: new Date(msg.createdAt),
          status: msg.status,
          type: msg.type,
        };
        handleNewMessage(msg.chatId, formattedMessage, msg.senderId);
        // Also try to create chat if it doesn't exist
        createChatForNewMessage(msg.chatId, formattedMessage, msg.senderId);
      });
      
      if (channel) {
        supabaseUnsubscribe = () => supabaseMessages.unsubscribe(channel);
      }
    }

    // Listen for new chats (when someone sends you a message for the first time)
    const unsubNewChat = liveChatService.onNewChat((newChat) => {
      setChats((prev) => {
        // Check if chat already exists
        if (prev.some(c => c.id === newChat.id)) {
          return prev;
        }
        
        const updated = [newChat, ...prev];
        saveUserChats(updated);
        return updated;
      });
    });
    
    return () => {
      unsubMessage();
      unsubNewChat();
      if (supabaseUnsubscribe) supabaseUnsubscribe();
    };
  }, [isLoading, selectedChatId]);

  // Save chats when they change
  useEffect(() => {
    if (!isLoading && chats.length > 0) {
      saveUserChats(chats);
    }
  }, [chats, isLoading]);

  const selectedChat = selectedChatId ? (chats.find(c => c.id === selectedChatId) || getChatById(selectedChatId)) : null;
  const unreadChats = chats.reduce((acc, chat) => acc + chat.unreadCount, 0);
  const missedCalls = calls.filter((call) => call.status === 'missed').length;

  const handleSelectChat = (chatId: string) => {
    setSelectedChatId(chatId);
    
    // Mark chat as read
    setChats((prev) => {
      const chatIndex = prev.findIndex(c => c.id === chatId);
      if (chatIndex === -1) return prev;
      
      const updated = [...prev];
      updated[chatIndex] = {
        ...updated[chatIndex],
        unreadCount: 0,
      };
      return updated;
    });
  };

  const handleBackFromChat = () => {
    setSelectedChatId(null);
  };

  const handleOpenSettings = () => {
    setIsSettingsOpen(true);
  };
  
  const handleChatsChange = (newChats: Chat[]) => {
    setChats(newChats);
    saveUserChats(newChats);
  };

  const renderContent = () => {
    // If a chat is selected, show the conversation view
    if (selectedChat && activeTab === 'chats') {
      return (
        <ConversationView chat={selectedChat} onBack={handleBackFromChat} />
      );
    }

    // Otherwise show the appropriate tab content
    switch (activeTab) {
      case 'chats':
        return (
          <ChatList
            selectedChatId={selectedChatId}
            onSelectChat={handleSelectChat}
            onOpenSettings={handleOpenSettings}
            chats={chats}
            onChatsChange={handleChatsChange}
          />
        );
      case 'status':
        return <StatusList />;
      case 'calls':
        return <CallsList />;
      default:
        return null;
    }
  };

  // Show loading state while checking auth
  if (isLoading) {
    return (
      <div className="h-dvh flex items-center justify-center">
        <div className="animate-spin w-8 h-8 border-4 border-primary border-t-transparent rounded-full" />
      </div>
    );
  }

  return (
    <div className="h-dvh flex flex-col overflow-hidden">
      {/* Main content area */}
      <main className={cn(
        'flex-1 overflow-hidden relative',
        selectedChat ? '' : ''
      )}>
        {renderContent()}
      </main>

      <SettingsModal open={isSettingsOpen} onOpenChange={setIsSettingsOpen} />

      {/* Tab bar - hide when viewing a conversation */}
      {!selectedChat && (
        <TabBar
          activeTab={activeTab}
          onTabChange={setActiveTab}
          unreadChats={unreadChats}
          missedCalls={missedCalls}
        />
      )}
    </div>
  );
}