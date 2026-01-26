'use client';

import { cn } from '@/lib/utils';
import type { Message } from '@/lib/types';
import { formatTime } from '@/lib/format';
import { Check, CheckCheck, Timer } from 'lucide-react';

interface MessageBubbleProps {
  message: Message;
  isOwn: boolean;
  showTimestamp?: boolean;
  searchQuery?: string;
}

// Helper function to highlight search matches
function highlightText(text: string, query: string, isOwn: boolean) {
  if (!query || query.trim() === '') {
    return text;
  }

  const parts = text.split(new RegExp(`(${query})`, 'gi'));
  
  return parts.map((part, index) => {
    if (part.toLowerCase() === query.toLowerCase()) {
      return (
        <mark
          key={index}
          className={cn(
            'bg-yellow-400/80 text-black rounded px-0.5',
            isOwn ? 'bg-yellow-300' : 'bg-yellow-400'
          )}
        >
          {part}
        </mark>
      );
    }
    return part;
  });
}

export function MessageBubble({ message, isOwn, showTimestamp = true, searchQuery }: MessageBubbleProps) {
  const getStatusIcon = () => {
    if (!isOwn) return null;
    
    switch (message.status) {
      case 'read':
        return <CheckCheck className="h-3.5 w-3.5 text-primary-foreground/80" />;
      case 'delivered':
        return <CheckCheck className="h-3.5 w-3.5 text-primary-foreground/60" />;
      case 'sent':
        return <Check className="h-3.5 w-3.5 text-primary-foreground/60" />;
      case 'sending':
        return (
          <div className="h-3.5 w-3.5 rounded-full border-2 border-primary-foreground/40 border-t-transparent animate-spin" />
        );
      default:
        return null;
    }
  };

  return (
    <div
      className={cn(
        'flex w-full mb-1',
        isOwn ? 'justify-end' : 'justify-start'
      )}
    >
      <div
        className={cn(
          'max-w-[80%] px-4 py-2.5',
          'animate-in fade-in-0 slide-in-from-bottom-2 duration-300',
          isOwn ? 'message-bubble-sent' : 'message-bubble-received'
        )}
      >
        <p className={cn(
          'text-[15px] leading-relaxed',
          isOwn ? 'text-primary-foreground' : 'text-foreground'
        )}>
          {highlightText(message.content, searchQuery || '', isOwn)}
        </p>
        
        {showTimestamp && (
          <div className={cn(
            'flex items-center justify-end gap-1 mt-1',
            isOwn ? 'text-primary-foreground/70' : 'text-muted-foreground'
          )}>
            <span className="text-[11px]">
              {formatTime(message.timestamp)}
            </span>
            {message.expiresAt && (
              <Timer
                className={cn(
                  'h-3.5 w-3.5',
                  isOwn ? 'text-primary-foreground/70' : 'text-muted-foreground'
                )}
                aria-label="Disappearing message"
              />
            )}
            {getStatusIcon()}
          </div>
        )}
      </div>
    </div>
  );
}
