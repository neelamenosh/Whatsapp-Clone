'use client';

import { useState, useRef, useEffect } from 'react';
import { cn } from '@/lib/utils';
import { getCurrentUser } from '@/lib/auth-store';
import { isSupabaseConfigured } from '@/lib/supabase/client';
import * as supabaseUsers from '@/lib/supabase/users';
import { ChatListItem } from './chat-list-item';
import { NewChatModal } from './new-chat-modal';
import { ProfileModal } from '@/components/profile/profile-modal';
import { ServerSidebar } from '@/components/shared/server-sidebar';
import {
  Search,
  ChevronRight,
  Sparkles,
  Volume2,
  Users,
  ChevronDown,
  Plus
} from 'lucide-react';
import type { Chat, User } from '@/lib/types';

interface ChatListProps {
  selectedChatId: string | null;
  onSelectChat: (chatId: string) => void;
  onOpenSettings: () => void;
  chats: Chat[];
  onChatsChange: (chats: Chat[]) => void;
}

export function ChatList({ selectedChatId, onSelectChat, onOpenSettings, chats, onChatsChange }: ChatListProps) {
  const [searchQuery, setSearchQuery] = useState('');
  const [isNewChatOpen, setIsNewChatOpen] = useState(false);
  const [isProfileOpen, setIsProfileOpen] = useState(false);
  const [currentUser, setCurrentUser] = useState<User | null>(null);
  const [isSummaryExpanded, setIsSummaryExpanded] = useState(true);
  const [isVoiceExpanded, setIsVoiceExpanded] = useState(true);

  // Load current user
  useEffect(() => {
    const user = getCurrentUser();
    setCurrentUser(user);
  }, [isProfileOpen]);

  // Subscribe to status updates
  useEffect(() => {
    if (!isSupabaseConfigured()) return;
    const unsubscribe = supabaseUsers.subscribeToAllUsersStatus((userId, status, lastSeen) => {
      onChatsChange(chats.map(chat => {
        const participantIndex = chat.participants.findIndex(p => p.id === userId);
        if (participantIndex !== -1) {
          const updatedParticipants = [...chat.participants];
          updatedParticipants[participantIndex] = {
            ...updatedParticipants[participantIndex],
            status,
            lastSeen: new Date(lastSeen),
          };
          return { ...chat, participants: updatedParticipants };
        }
        return chat;
      }));
    });
    return () => { if (unsubscribe) unsubscribe(); };
  }, [chats, onChatsChange]);

  const filteredChats = chats.filter((chat) => {
    const name = chat.type === 'group'
      ? 'Design Team'
      : chat.participants[0].name;
    return name.toLowerCase().includes(searchQuery.toLowerCase());
  });

  return (
    <div className="flex h-full overflow-hidden">
      {/* Sidebar */}
      <ServerSidebar activeId="home" />

      {/* Main Content */}
      <div className="flex-1 flex flex-col min-w-0 bg-background overflow-hidden relative">
        {/* User Header */}
        <div className="p-4">
          <button
            type="button"
            onClick={() => setIsProfileOpen(true)}
            className="flex items-center justify-between w-full p-2 rounded-2xl hover:bg-muted/50 transition-colors"
          >
            <div className="flex items-center gap-3">
              <div className="relative">
                <img
                  src={currentUser?.avatar || "/placeholder.svg"}
                  alt={currentUser?.name}
                  className="w-12 h-12 rounded-full object-cover"
                  crossOrigin="anonymous"
                />
                <div className="absolute bottom-0 right-0 w-3 h-3 rounded-full bg-primary border-2 border-background" />
              </div>
              <div className="flex flex-col text-left">
                <span className="font-bold text-foreground leading-tight">{currentUser?.name || "User"}</span>
                <span className="text-xs text-muted-foreground">@marcaum.eth</span>
              </div>
            </div>
            <ChevronRight className="h-5 w-5 text-muted-foreground" />
          </button>
        </div>

        {/* Search */}
        <div className="px-4 mb-4">
          <div className="relative">
            <Search className="absolute left-4 top-1/2 -translate-y-1/2 h-5 w-5 text-muted-foreground" />
            <input
              type="text"
              placeholder="Search or ask the AI"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              className="w-full glass-input pl-12 pr-4 py-3 text-sm text-foreground bg-muted/30 border-none rounded-2xl focus:bg-muted/50 transition-colors"
            />
          </div>
        </div>

        {/* Scrollable Area */}
        <div className="flex-1 overflow-y-auto scrollbar-hide">
          {/* Summary Section */}
          <div className="mb-4">
            <button
              type="button"
              onClick={() => setIsSummaryExpanded(!isSummaryExpanded)}
              className="flex items-center justify-between w-full px-4 py-2 text-xs font-semibold text-muted-foreground uppercase tracking-wider"
            >
              <span>Summary</span>
              <ChevronDown className={cn("h-4 w-4 transition-transform", !isSummaryExpanded && "-rotate-90")} />
            </button>
            {isSummaryExpanded && (
              <div className="px-4 py-2">
                <button
                  type="button"
                  className="flex items-center justify-center gap-2 w-full py-3 bg-primary/10 text-primary rounded-2xl font-medium hover:bg-primary/15 transition-colors border border-primary/20"
                >
                  <Sparkles className="h-4 w-4" />
                  <span>Summarize the day</span>
                </button>
              </div>
            )}
          </div>

          {/* Voice Section */}
          <div className="mb-4">
            <button
              type="button"
              onClick={() => setIsVoiceExpanded(!isVoiceExpanded)}
              className="flex items-center justify-between w-full px-4 py-2 text-xs font-semibold text-muted-foreground uppercase tracking-wider"
            >
              <span>Voice</span>
              <ChevronDown className={cn("h-4 w-4 transition-transform", !isVoiceExpanded && "-rotate-90")} />
            </button>
            {isVoiceExpanded && (
              <div className="px-2 space-y-1">
                <button
                  type="button"
                  className="flex items-center gap-3 w-full p-3 rounded-2xl hover:bg-muted/50 transition-colors group"
                >
                  <div className="w-10 h-10 rounded-full bg-primary/10 flex items-center justify-center">
                    <Volume2 className="h-5 w-5 text-primary" />
                  </div>
                  <div className="flex-1 text-left">
                    <p className="text-sm font-semibold">Success Squad</p>
                    <div className="flex -space-x-2 mt-1">
                      {[1, 2, 3].map((i) => (
                        <img
                          key={i}
                          src={`https://images.unsplash.com/photo-${1500000000000 + i}?w=50&h=50&fit=crop&crop=face`}
                          alt="participant"
                          className="w-5 h-5 rounded-full border border-background"
                        />
                      ))}
                    </div>
                  </div>
                </button>
              </div>
            )}
          </div>

          {/* Chats Section */}
          <div>
            <div className="px-4 py-2 text-xs font-semibold text-muted-foreground uppercase tracking-wider">
              Chats
            </div>
            <div className="px-2">
              {filteredChats.map((chat) => (
                <ChatListItem
                  key={chat.id}
                  chat={chat}
                  isSelected={selectedChatId === chat.id}
                  onClick={() => onSelectChat(chat.id)}
                  onChatUpdate={(updatedChat) => {
                    onChatsChange(chats.map(c => c.id === updatedChat.id ? updatedChat : c));
                  }}
                  onChatDelete={(chatId) => {
                    onChatsChange(chats.filter(c => c.id !== chatId));
                  }}
                />
              ))}
            </div>
          </div>
        </div>

        {/* Floating Action Button */}
        <button
          type="button"
          onClick={() => setIsNewChatOpen(true)}
          className="absolute bottom-6 right-6 w-14 h-14 bg-primary text-white rounded-full flex items-center justify-center shadow-lg shadow-primary/30 hover:scale-110 active:scale-95 transition-all z-10"
        >
          <Plus className="h-6 w-6" />
        </button>
      </div>

      {/* Modals */}
      <NewChatModal
        open={isNewChatOpen}
        onOpenChange={setIsNewChatOpen}
        existingChats={chats}
        onStartChat={(newChat) => {
          const exists = chats.find(c => c.id === newChat.id);
          if (!exists) onChatsChange([newChat, ...chats]);
          onSelectChat(newChat.id);
        }}
      />
      <ProfileModal
        open={isProfileOpen}
        onOpenChange={setIsProfileOpen}
      />
    </div>
  );
}
