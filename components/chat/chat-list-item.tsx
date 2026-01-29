'use client';

import { cn } from '@/lib/utils';
import type { Chat } from '@/lib/types';
import { formatDistanceToNow } from '@/lib/format';
import { getCurrentUser } from '@/lib/auth-store';
import {
  Pin,
  VolumeX,
  Check,
  CheckCheck,
  Clock
} from 'lucide-react';

interface ChatListItemProps {
  chat: Chat;
  isSelected: boolean;
  onClick: () => void;
  onChatUpdate?: (updatedChat: Chat) => void;
  onChatDelete?: (chatId: string) => void;
}

export function ChatListItem({ chat, isSelected, onClick }: ChatListItemProps) {
  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;
  const loggedInUser = getCurrentUser();

  const getStatusIcon = () => {
    if (!chat.lastMessage || !loggedInUser || chat.lastMessage.senderId !== loggedInUser.id) return null;

    switch (chat.lastMessage.status) {
      case 'read':
        return <CheckCheck className="w-4 h-4 text-primary shrink-0" />;
      case 'delivered':
        return <CheckCheck className="w-4 h-4 text-muted-foreground shrink-0" />;
      case 'sent':
        return <Check className="w-4 h-4 text-muted-foreground shrink-0" />;
      case 'sending':
        return <Clock className="h-3.5 w-3.5 text-muted-foreground/60 shrink-0" />;
      default:
        return null;
    }
  };

  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        'w-full flex items-center gap-3 p-3 rounded-2xl transition-all duration-200 text-left mb-1',
        isSelected
          ? 'bg-primary/10'
          : 'hover:bg-muted/50'
      )}
    >
      {/* Avatar */}
      <div className="relative shrink-0">
        <img
          src={participant.avatar || "/placeholder.svg"}
          alt={displayName}
          className="w-12 h-12 rounded-full object-cover"
          crossOrigin="anonymous"
        />
        {participant.status === 'online' && (
          <div className="absolute bottom-0 right-0 w-3 h-3 rounded-full bg-primary border-2 border-background" />
        )}
      </div>

      {/* Content */}
      <div className="flex-1 min-w-0">
        <div className="flex items-center justify-between mb-0.5">
          <div className="flex items-center gap-1.5 min-w-0">
            <span className="font-bold text-foreground text-sm truncate">
              {displayName}
            </span>
            {chat.isPinned && (
              <Pin className="h-3 w-3 text-muted-foreground fill-muted-foreground shrink-0" />
            )}
            {chat.isMuted && (
              <VolumeX className="h-3 w-3 text-muted-foreground shrink-0" />
            )}
          </div>
          <span className={cn(
            'text-[10px] shrink-0',
            chat.unreadCount > 0 ? 'text-primary font-bold' : 'text-muted-foreground font-medium'
          )}>
            {chat.lastMessage && formatDistanceToNow(chat.lastMessage.timestamp)}
          </span>
        </div>

        <div className="flex items-center justify-between">
          <div className="flex items-center gap-1 min-w-0 flex-1">
            {getStatusIcon()}
            <p className={cn(
              'text-xs truncate',
              chat.unreadCount > 0 ? 'text-foreground font-semibold' : 'text-muted-foreground font-medium'
            )}>
              {chat.lastMessage?.content || 'No messages yet'}
            </p>
          </div>

          {chat.unreadCount > 0 && (
            <div className="ml-2 badge-unread">
              {chat.unreadCount}
            </div>
          )}
        </div>
      </div>
    </button>
  );
}
