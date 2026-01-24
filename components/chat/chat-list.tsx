'use client';

import { useState } from 'react';
import { cn } from '@/lib/utils';
import { chats } from '@/lib/mock-data';
import { ChatListItem } from './chat-list-item';
import { Search, Camera, MoreHorizontal, Plus } from 'lucide-react';

interface ChatListProps {
  selectedChatId: string | null;
  onSelectChat: (chatId: string) => void;
}

export function ChatList({ selectedChatId, onSelectChat }: ChatListProps) {
  const [searchQuery, setSearchQuery] = useState('');

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
          <h1 className="text-2xl font-bold text-foreground">Chats</h1>
          <div className="flex items-center gap-2">
            <button 
              type="button"
              className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
              aria-label="Camera"
            >
              <Camera className="h-5 w-5" />
            </button>
            <button 
              type="button"
              className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
              aria-label="More options"
            >
              <MoreHorizontal className="h-5 w-5" />
            </button>
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

      {/* Floating Action Button */}
      <button
        type="button"
        className={cn(
          'absolute bottom-24 right-4 w-14 h-14 rounded-full',
          'bg-primary text-primary-foreground',
          'shadow-lg shadow-primary/30',
          'flex items-center justify-center',
          'transition-all duration-300 hover:scale-110 active:scale-95'
        )}
        aria-label="New chat"
      >
        <Plus className="h-6 w-6" />
      </button>
    </div>
  );
}
