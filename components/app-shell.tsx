'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { cn } from '@/lib/utils';
import type { TabType, Chat } from '@/lib/types';
import { calls, getChatById, setCurrentUserData } from '@/lib/mock-data';
import { getCurrentUser, isDatabaseConfigured } from '@/lib/auth-store';
import { getUserChats, saveUserChats } from '@/lib/chat-store';
import { getLiveChatService } from '@/lib/live-chat';
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
      
      setIsLoading(false);
    }
  }, [router]);

  // Listen for new messages and update chat list
  useEffect(() => {
    if (isLoading) return;
    
    const liveChatService = getLiveChatService();
    
    const unsubMessage = liveChatService.onMessage((chatId, message) => {
      setChats((prev) => {
        const chatIndex = prev.findIndex(c => c.id === chatId);
        if (chatIndex === -1) return prev;
        
        const updated = [...prev];
        updated[chatIndex] = {
          ...updated[chatIndex],
          lastMessage: message,
          updatedAt: new Date(),
          unreadCount: selectedChatId === chatId 
            ? updated[chatIndex].unreadCount 
            : updated[chatIndex].unreadCount + 1,
        };
        
        // Sort by updatedAt
        updated.sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
        
        // Persist to localStorage
        saveUserChats(updated);
        
        return updated;
      });
    });

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