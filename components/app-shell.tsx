'use client';

import { useState, useEffect, useRef, useCallback } from 'react';
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
  
  // Use ref to track processed message IDs to prevent duplicate updates
  const processedMessageIds = useRef(new Set<string>());
  // Use ref to store current selectedChatId for use in callbacks
  const selectedChatIdRef = useRef<string | null>(null);
  
  // Keep ref in sync with state
  useEffect(() => {
    selectedChatIdRef.current = selectedChatId;
  }, [selectedChatId]);

  // Handle browser back button for in-app navigation
  useEffect(() => {
    const handlePopState = (event: PopStateEvent) => {
      // If we're in a chat and user presses back, go to chat list instead of leaving the app
      if (event.state?.inChat) {
        // User navigated forward to a chat, do nothing special
      } else if (selectedChatIdRef.current) {
        // User pressed back while in a chat - close the chat
        setSelectedChatId(null);
      }
    };

    window.addEventListener('popstate', handlePopState);
    
    // Replace current history state on mount to mark the base state
    if (!window.history.state?.appBase) {
      window.history.replaceState({ appBase: true }, '');
    }

    return () => {
      window.removeEventListener('popstate', handlePopState);
    };
  }, []);

  // Check if user is logged in and load chats
  useEffect(() => {
    const currentUser = getCurrentUser();
    if (!currentUser) {
      router.push('/login');
    } else {
      // Set current user data in mock-data
      setCurrentUserData(currentUser);
      
      // Load user's chats from localStorage first
      const localChats = getUserChats();
      setChats(localChats);
      
      // Initialize live chat service
      getLiveChatService();
      
      // Initialize presence tracking (sets user online and tracks visibility)
      const cleanupPresence = initPresenceTracking();
      
      // Load chats from Supabase to get messages from other users
      const loadSupabaseChats = async () => {
        if (isSupabaseConfigured()) {
          try {
            // Get all chats/conversations from Supabase
            const supabaseChats = await supabaseMessages.getUserChats(currentUser.id);
            
            if (supabaseChats.length > 0) {
              // For each chat, get the other user's info and create chat objects
              const newChats: Chat[] = [];
              
              for (const chatData of supabaseChats) {
                // Check if this chat already exists in local chats
                const existsLocally = localChats.some(c => 
                  c.participants[0]?.id === chatData.recipientId ||
                  c.id === chatData.lastMessage.chatId
                );
                
                if (!existsLocally) {
                  // Fetch the other user's info
                  const otherUser = await supabaseUsers.getUserById(chatData.recipientId);
                  
                  if (otherUser) {
                    const chat: Chat = {
                      id: chatData.lastMessage.chatId,
                      type: 'individual',
                      participants: [{
                        id: otherUser.id,
                        name: otherUser.displayName,
                        email: otherUser.email,
                        avatar: otherUser.avatar || 'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face',
                        status: otherUser.status || 'online',
                        about: otherUser.bio || 'Available',
                        phone: otherUser.phone,
                      }],
                      lastMessage: {
                        id: chatData.lastMessage.id,
                        senderId: chatData.lastMessage.senderId,
                        content: chatData.lastMessage.content,
                        timestamp: new Date(chatData.lastMessage.createdAt),
                        status: chatData.lastMessage.status,
                        type: chatData.lastMessage.type,
                      },
                      unreadCount: chatData.lastMessage.senderId !== currentUser.id ? 1 : 0,
                      isPinned: false,
                      isMuted: false,
                      updatedAt: new Date(chatData.lastMessage.createdAt),
                    };
                    newChats.push(chat);
                  }
                }
              }
              
              if (newChats.length > 0) {
                setChats(prev => {
                  // Merge new chats with existing, avoiding duplicates
                  const merged = [...prev];
                  for (const newChat of newChats) {
                    if (!merged.some(c => c.id === newChat.id || c.participants[0]?.id === newChat.participants[0]?.id)) {
                      merged.push(newChat);
                    }
                  }
                  // Sort by updatedAt
                  merged.sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
                  return merged;
                });
              }
            }
            
            // Mark all pending messages as delivered since user is now online
            // This updates the sender's messages from 'sent' (single tick) to 'delivered' (double tick)
            await supabaseMessages.markMessagesAsDelivered(currentUser.id);
          } catch (err) {
            console.error('Failed to load Supabase chats:', err);
          }
        }
      };
      
      loadSupabaseChats();
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
      
      // Skip if already processed
      if (processedMessageIds.current.has(message.id)) return;
      processedMessageIds.current.add(message.id);
      
      // Limit set size to prevent memory leak
      if (processedMessageIds.current.size > 1000) {
        const arr = Array.from(processedMessageIds.current);
        processedMessageIds.current = new Set(arr.slice(-500));
      }
      
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
        const currentSelectedId = selectedChatIdRef.current;
        updated[chatIndex] = {
          ...updated[chatIndex],
          lastMessage: message,
          updatedAt: new Date(),
          unreadCount: currentSelectedId === chatId || currentSelectedId === updated[chatIndex].id
            ? updated[chatIndex].unreadCount 
            : (senderId !== currentUserNow?.id ? updated[chatIndex].unreadCount + 1 : updated[chatIndex].unreadCount),
        };
        
        // Sort by updatedAt
        updated.sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
        
        // Persistence is handled by debounced useEffect
        
        return updated;
      });
    };
    
    // Helper to create a new chat when message comes from unknown user
    const createChatForNewMessage = async (chatId: string, message: any, senderId: string) => {
      const currentUserNow = getCurrentUser();
      if (!currentUserNow || senderId === currentUserNow.id) return;
      
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
      
      // Use functional update to check current state and avoid stale closure
      setChats((prev) => {
        // Check if chat already exists in current state
        const existingChat = prev.find(c => {
          if (c.id === chatId) return true;
          if (c.type !== 'individual') return false;
          const participant = c.participants[0];
          return participant.id === senderId || getConsistentChatId(currentUserNow.id, participant.id) === chatId;
        });
        
        if (existingChat) {
          return prev; // Don't add, already exists
        }
        
        // Double check it doesn't exist by participant ID
        if (prev.some(c => c.participants[0]?.id === senderId)) {
          return prev;
        }
        
        const updated = [newChat, ...prev];
        // Sort by updatedAt
        updated.sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
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
        // Persistence handled by debounced useEffect
        return updated;
      });
    });
    
    return () => {
      unsubMessage();
      unsubNewChat();
      if (supabaseUnsubscribe) supabaseUnsubscribe();
    };
  }, [isLoading]);  // Removed selectedChatId to prevent re-subscribing

  // Debounced save - only save after changes settle to prevent flickering
  const saveTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  
  useEffect(() => {
    if (!isLoading && chats.length > 0) {
      // Clear any pending save
      if (saveTimeoutRef.current) {
        clearTimeout(saveTimeoutRef.current);
      }
      // Debounce save to prevent multiple rapid saves causing re-renders
      saveTimeoutRef.current = setTimeout(() => {
        saveUserChats(chats);
      }, 500);
    }
    
    return () => {
      if (saveTimeoutRef.current) {
        clearTimeout(saveTimeoutRef.current);
      }
    };
  }, [chats, isLoading]);

  const selectedChat = selectedChatId ? (chats.find(c => c.id === selectedChatId) || getChatById(selectedChatId)) : null;
  const unreadChats = chats.reduce((acc, chat) => acc + chat.unreadCount, 0);
  const missedCalls = calls.filter((call) => call.status === 'missed').length;

  const handleSelectChat = (chatId: string) => {
    setSelectedChatId(chatId);
    
    // Push state to browser history so back button returns to chat list
    window.history.pushState({ inChat: true, chatId }, '');
    
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
    
    // Go back in history if we pushed a state when entering the chat
    if (window.history.state?.inChat) {
      window.history.back();
    }
  };

  const handleOpenSettings = () => {
    setIsSettingsOpen(true);
  };
  
  const handleChatsChange = (newChats: Chat[]) => {
    setChats(newChats);
    saveUserChats(newChats);
  };
  
  // Handler for when a message is sent from the conversation view
  const handleMessageSent = useCallback((chatId: string, message: any) => {
    setChats((prev) => {
      const chatIndex = prev.findIndex(c => c.id === chatId || c.id === selectedChatId);
      if (chatIndex === -1) return prev;
      
      const updated = [...prev];
      updated[chatIndex] = {
        ...updated[chatIndex],
        lastMessage: message,
        updatedAt: new Date(),
      };
      
      // Sort by updatedAt to move this chat to the top
      updated.sort((a, b) => new Date(b.updatedAt).getTime() - new Date(a.updatedAt).getTime());
      
      return updated;
    });
  }, [selectedChatId]);

  const renderContent = () => {
    // If a chat is selected, show the conversation view
    if (selectedChat && activeTab === 'chats') {
      return (
        <ConversationView 
          chat={selectedChat} 
          onBack={handleBackFromChat} 
          onMessageSent={handleMessageSent}
        />
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