'use client';

import { cn } from '@/lib/utils';
import type { Message } from '@/lib/types';
import { formatTime } from '@/lib/format';
import { Clock, Check, CheckCheck } from 'lucide-react';

interface MessageBubbleProps {
  message: Message;
  isOwn: boolean;
  showTimestamp?: boolean;
}

export function MessageBubble({ message, isOwn, showTimestamp = true }: MessageBubbleProps) {
  const getStatusIcon = () => {
    if (!isOwn) return null;

    switch (message.status) {
      case 'read':
        return <CheckCheck className="w-3.5 h-3.5 text-white/90" />;
      case 'delivered':
        return <CheckCheck className="w-3.5 h-3.5 text-white/60" />;
      case 'sent':
        return <Check className="w-3.5 h-3.5 text-white/60" />;
      case 'sending':
        return <Clock className="h-3 w-3 text-white/40" />;
      default:
        return null;
    }
  };

  return (
    <div
      className={cn(
        'flex w-full mb-1 group',
        isOwn ? 'justify-end' : 'justify-start'
      )}
    >
      <div
        className={cn(
          'max-w-[85%] sm:max-w-[70%] px-4 py-2 relative',
          'animate-in fade-in-0 duration-300',
          isOwn
            ? 'bg-primary text-white rounded-[1.25rem] rounded-tr-[0.25rem] shadow-sm'
            : 'bg-muted/50 text-foreground rounded-[1.25rem] rounded-tl-[0.25rem] border border-border/50'
        )}
      >
        <div className="flex flex-col">
          <p className="text-[14px] leading-relaxed">
            {message.content}
          </p>

          {showTimestamp && (
            <div className={cn(
              'flex items-center justify-end gap-1 mt-0.5',
              isOwn ? 'text-white/60' : 'text-muted-foreground'
            )}>
              <span className="text-[10px] whitespace-nowrap">
                {formatTime(message.timestamp)}
              </span>
              {getStatusIcon()}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
