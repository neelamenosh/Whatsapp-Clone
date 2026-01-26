'use client';

import { useState, useRef, useEffect } from 'react';
import { cn } from '@/lib/utils';
import { chats as initialChats } from '@/lib/mock-data';
import { getCurrentUser } from '@/lib/auth-store';
import { isSupabaseConfigured } from '@/lib/supabase/client';
import * as supabaseUsers from '@/lib/supabase/users';
import { ChatListItem } from './chat-list-item';
import { NewChatModal } from './new-chat-modal';
import { ProfileModal } from '@/components/profile/profile-modal';
import { Search, Camera, MoreHorizontal, Plus, Users, Globe, Tag, Smartphone, Settings } from 'lucide-react';
import type { Chat, User } from '@/lib/types';

interface ChatListProps {
  selectedChatId: string | null;
  onSelectChat: (chatId: string) => void;
  onOpenSettings: () => void;
  chats: Chat[];
  onChatsChange: (chats: Chat[]) => void;
}

const menuItems = [
  { icon: Users, label: 'New Group' },
  { icon: Globe, label: 'Communities' },
  { icon: Tag, label: 'Labels' },
  { icon: Smartphone, label: 'Linked devices' },
  { icon: Settings, label: 'Settings' },
];

export function ChatList({ selectedChatId, onSelectChat, onOpenSettings, chats, onChatsChange }: ChatListProps) {
  const [searchQuery, setSearchQuery] = useState('');
  const [isMenuOpen, setIsMenuOpen] = useState(false);
  const [isNewChatOpen, setIsNewChatOpen] = useState(false);
  const [isProfileOpen, setIsProfileOpen] = useState(false);
  const [currentUser, setCurrentUser] = useState<User | null>(null);
  const menuRef = useRef<HTMLDivElement>(null);

  // Load current user
  useEffect(() => {
    const user = getCurrentUser();
    setCurrentUser(user);
  }, [isProfileOpen]); // Refresh when profile closes

  // Subscribe to all users' status changes via Supabase
  useEffect(() => {
    if (!isSupabaseConfigured()) return;
    
    const unsubscribe = supabaseUsers.subscribeToAllUsersStatus((userId, status, lastSeen) => {
      // Update the status of participants in chats
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

    return () => {
      if (unsubscribe) unsubscribe();
    };
  }, [chats, onChatsChange]);

  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        setIsMenuOpen(false);
      }
    }
    if (isMenuOpen) {
      document.addEventListener('mousedown', handleClickOutside);
    }
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, [isMenuOpen]);

  const filteredChats = chats.filter((chat) => {
    const name = chat.type === 'group' 
      ? 'Design Team' 
      : chat.participants[0].name;
    return name.toLowerCase().includes(searchQuery.toLowerCase());
  });

  const pinnedChats = filteredChats.filter((chat) => chat.isPinned);
  const regularChats = filteredChats.filter((chat) => !chat.isPinned);

  return (
    <div className="flex flex-col h-full">
      {/* Header */}
      <div className="p-4 pb-2">
        <div className="flex items-center justify-between mb-4">
          {/* User Avatar and Title */}
          <div className="flex items-center gap-3">
            <button
              type="button"
              onClick={() => setIsProfileOpen(true)}
              className="relative group"
              aria-label="Open profile"
            >
              <div className={cn(
                'w-10 h-10 rounded-full overflow-hidden',
                'ring-2 ring-glass-border/50',
                'transition-all duration-200',
                'group-hover:ring-primary group-hover:scale-105'
              )}>
                <img
                  src={currentUser?.avatar || "/placeholder.svg"}
                  alt={currentUser?.name || "Profile"}
                  className="w-full h-full object-cover"
                  crossOrigin="anonymous"
                />
              </div>
              {currentUser?.status === 'online' && (
                <div className="absolute bottom-0 right-0 w-3 h-3 rounded-full bg-online border-2 border-background" />
              )}
            </button>
            <h1 className="text-2xl font-bold text-foreground">Chats</h1>
          </div>
          <div className="flex items-center gap-2">
            <button 
              type="button"
              className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
              aria-label="Camera"
            >
              <Camera className="h-5 w-5" />
            </button>
            <div className="relative" ref={menuRef}>
              <button 
                type="button"
                className={cn(
                  "glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground",
                  isMenuOpen && "text-foreground"
                )}
                aria-label="More options"
                onClick={() => setIsMenuOpen(!isMenuOpen)}
              >
                <MoreHorizontal className="h-5 w-5" />
              </button>

              {/* Liquid Glass Dropdown Menu - Green Accent */}
              <div
                className={cn(
                  "absolute right-0 top-full mt-2 w-52 py-2 rounded-2xl z-50",
                  "origin-top-right transition-all duration-300 ease-out",
                  isMenuOpen 
                    ? "opacity-100 scale-100 translate-y-0" 
                    : "opacity-0 scale-95 -translate-y-2 pointer-events-none"
                )}
                style={{
                  background: 'linear-gradient(145deg, rgba(255,255,255,0.85) 0%, rgba(240,253,244,0.7) 50%, rgba(255,255,255,0.8) 100%)',
                  backdropFilter: 'blur(24px)',
                  WebkitBackdropFilter: 'blur(24px)',
                  border: '1px solid rgba(134,239,172,0.3)',
                  boxShadow: `
                    0 16px 48px rgba(22,163,74,0.1),
                    0 4px 16px rgba(0,0,0,0.08),
                    inset 0 1px 0 rgba(255,255,255,0.8),
                    inset 0 -1px 0 rgba(187,247,208,0.3)
                  `,
                }}
              >
                {/* Top highlight for glass refraction */}
                <span 
                  className="absolute top-2 left-4 right-4 h-px pointer-events-none"
                  style={{
                    background: 'linear-gradient(90deg, transparent, rgba(187,247,208,0.8), transparent)',
                  }}
                />
                
                {menuItems.map((item) => (
                  <button
                    key={item.label}
                    type="button"
                    className={cn(
                      "w-full flex items-center gap-3 px-4 py-2.5 text-sm text-foreground/90",
                      "hover:bg-green-50/60 dark:hover:bg-green-900/20 transition-colors duration-200",
                      "active:bg-green-100/60 dark:active:bg-green-900/30"
                    )}
                    onClick={() => {
                      setIsMenuOpen(false);
                      if (item.label === 'Settings') onOpenSettings();
                    }}
                  >
                    <item.icon className="h-5 w-5 text-primary" />
                    <span>{item.label}</span>
                  </button>
                ))}
              </div>
            </div>
          </div>
        </div>

        {/* Search */}
        <div className="relative">
          <Search className="absolute left-4 top-1/2 -translate-y-1/2 h-5 w-5 text-muted-foreground" />
          <input
            type="text"
            placeholder="Search conversations..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full glass-input pl-12 pr-4 py-3 text-sm text-foreground placeholder:text-muted-foreground"
          />
        </div>
      </div>

      {/* Chat List */}
      <div className="flex-1 overflow-y-auto px-2 pb-4 scrollbar-hide">
        {pinnedChats.length > 0 && (
          <div className="mb-2">
            <p className="px-4 py-2 text-xs font-medium text-muted-foreground uppercase tracking-wider">
              Pinned
            </p>
            {pinnedChats.map((chat) => (
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
        )}

        {regularChats.length > 0 && (
          <div>
            {pinnedChats.length > 0 && (
              <p className="px-4 py-2 text-xs font-medium text-muted-foreground uppercase tracking-wider">
                All Chats
              </p>
            )}
            {regularChats.map((chat) => (
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
        )}

        {filteredChats.length === 0 && (
          <div className="flex flex-col items-center justify-center py-12 text-center">
            <div className="w-16 h-16 rounded-full bg-muted/50 flex items-center justify-center mb-4">
              <Search className="h-8 w-8 text-muted-foreground" />
            </div>
            <p className="text-muted-foreground">No conversations found</p>
          </div>
        )}
      </div>

      {/* Floating Action Button - Green Glass Droplet */}
      <button
        type="button"
        onClick={() => setIsNewChatOpen(true)}
        className={cn(
          'absolute bottom-24 right-4 w-14 h-14 rounded-full',
          'flex items-center justify-center',
          'transition-all duration-300 hover:scale-110 active:scale-95',
          'group overflow-hidden'
        )}
        style={{
          background: 'linear-gradient(135deg, rgba(34,197,94,0.85) 0%, rgba(22,163,74,0.9) 50%, rgba(21,128,61,0.95) 100%)',
          backdropFilter: 'blur(20px)',
          WebkitBackdropFilter: 'blur(20px)',
          border: '1px solid rgba(255,255,255,0.3)',
          boxShadow: `
            0 8px 32px rgba(22,163,74,0.3),
            0 2px 8px rgba(0,0,0,0.15),
            inset 0 -4px 12px rgba(21,128,61,0.3),
            inset 0 4px 12px rgba(134,239,172,0.4),
            inset 2px 2px 4px rgba(255,255,255,0.3)
          `,
        }}
        aria-label="New chat"
      >
        {/* Refraction highlight - top left */}
        <span 
          className="absolute top-1.5 left-2 w-4 h-2 rounded-full opacity-90 pointer-events-none"
          style={{
            background: 'linear-gradient(135deg, rgba(187,247,208,0.9) 0%, rgba(134,239,172,0) 100%)',
          }}
        />
        {/* Secondary highlight - bottom curve */}
        <span 
          className="absolute bottom-2 right-2 w-6 h-3 rounded-full opacity-40 pointer-events-none"
          style={{
            background: 'radial-gradient(ellipse at center, rgba(187,247,208,0.6) 0%, transparent 70%)',
          }}
        />
        {/* Caustic light effect */}
        <span 
          className="absolute inset-0 rounded-full opacity-0 group-hover:opacity-100 transition-opacity duration-300 pointer-events-none"
          style={{
            background: 'radial-gradient(circle at 30% 30%, rgba(255,255,255,0.35) 0%, transparent 50%)',
          }}
        />
        <Plus className="h-6 w-6 text-white relative z-10 drop-shadow-sm" />
      </button>

      {/* New Chat Modal */}
      <NewChatModal
        open={isNewChatOpen}
        onOpenChange={setIsNewChatOpen}
        existingChats={chats}
        onStartChat={(newChat) => {
          // Check if chat already exists
          const exists = chats.find(c => c.id === newChat.id);
          if (!exists) {
            onChatsChange([newChat, ...chats]);
          }
          onSelectChat(newChat.id);
        }}
      />

      {/* Profile Modal */}
      <ProfileModal
        open={isProfileOpen}
        onOpenChange={setIsProfileOpen}
      />
    </div>
  );
}
