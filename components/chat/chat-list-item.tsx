'use client';

import { cn } from '@/lib/utils';
import type { Chat } from '@/lib/types';
import { formatDistanceToNow } from '@/lib/format';
import { Check, CheckCheck, Pin, VolumeX } from 'lucide-react';

interface ChatListItemProps {
  chat: Chat;
  isSelected: boolean;
  onClick: () => void;
}

export function ChatListItem({ chat, isSelected, onClick }: ChatListItemProps) {
  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;

  const getStatusIcon = () => {
    if (!chat.lastMessage || chat.lastMessage.senderId !== 'current-user') return null;
    
    switch (chat.lastMessage.status) {
      case 'read':
        return <CheckCheck className="h-4 w-4 text-primary" />;
      case 'delivered':
        return <CheckCheck className="h-4 w-4 text-muted-foreground" />;
      case 'sent':
        return <Check className="h-4 w-4 text-muted-foreground" />;
      default:
        return null;
    }
  };

  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        'w-full flex items-center gap-3 p-4 rounded-2xl transition-all duration-300 text-left',
        'hover:scale-[1.01] active:scale-[0.99]',
        isSelected 
          ? 'glass-card bg-primary/10' 
          : 'hover:bg-card/50'
      )}
    >
      {/* Avatar with online indicator */}
      <div className="relative shrink-0">
        <div className={cn(
          'w-14 h-14 rounded-full overflow-hidden',
          'ring-2 ring-glass-border/50',
          'shadow-lg'
        )}>
          <img
            src={participant.avatar || "/placeholder.svg"}
            alt={displayName}
            className="w-full h-full object-cover"
            crossOrigin="anonymous"
          />
        </div>
        {participant.status === 'online' && (
          <div className="absolute bottom-0 right-0 w-4 h-4 rounded-full bg-online border-2 border-background online-pulse" />
        )}
        {isGroup && (
          <div className="absolute -bottom-1 -right-1 w-5 h-5 rounded-full bg-primary flex items-center justify-center text-[10px] text-primary-foreground font-medium">
            {chat.participants.length}
          </div>
        )}
      </div>

      {/* Content */}
      <div className="flex-1 min-w-0">
        <div className="flex items-center justify-between mb-1">
          <div className="flex items-center gap-2">
            <span className="font-semibold text-foreground truncate">
              {displayName}
            </span>
            {chat.isPinned && (
              <Pin className="h-3 w-3 text-muted-foreground fill-muted-foreground" />
            )}
            {chat.isMuted && (
              <VolumeX className="h-3 w-3 text-muted-foreground" />
            )}
          </div>
          <span className={cn(
            'text-xs shrink-0',
            chat.unreadCount > 0 ? 'text-primary font-medium' : 'text-muted-foreground'
          )}>
            {chat.lastMessage && formatDistanceToNow(chat.lastMessage.timestamp)}
          </span>
        </div>
        
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-1 min-w-0 flex-1">
            {getStatusIcon()}
            <p className={cn(
              'text-sm truncate',
              chat.unreadCount > 0 ? 'text-foreground font-medium' : 'text-muted-foreground'
            )}>
              {chat.lastMessage?.content || 'No messages yet'}
            </p>
          </div>
          
          {chat.unreadCount > 0 && (
            <div className="ml-2 min-w-[20px] h-5 px-1.5 rounded-full bg-primary flex items-center justify-center badge-glow">
              <span className="text-xs font-semibold text-primary-foreground">
                {chat.unreadCount}
              </span>
            </div>
          )}
        </div>
      </div>
    </button>
  );
}
