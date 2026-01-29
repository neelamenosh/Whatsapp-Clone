'use client';

import { cn } from '@/lib/utils';
import type { Message } from '@/lib/types';
import { formatTime } from '@/lib/format';
import { Clock, Timer } from 'lucide-react';

interface MessageBubbleProps {
  message: Message;
  isOwn: boolean;
  showTimestamp?: boolean;
  searchQuery?: string;
}

// Custom SVG components for WhatsApp-style ticks
const SingleTick = ({ className }: { className?: string }) => (
  <svg 
    viewBox="0 0 16 11" 
    className={className}
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    <path 
      d="M11.071 0.929L5.5 6.5L3.429 4.429" 
      stroke="currentColor" 
      strokeWidth="1.8" 
      strokeLinecap="round" 
      strokeLinejoin="round"
      fill="none"
    />
  </svg>
);

const DoubleTick = ({ className }: { className?: string }) => (
  <svg 
    viewBox="0 0 20 11" 
    className={className}
    fill="none"
    xmlns="http://www.w3.org/2000/svg"
  >
    {/* First tick (back) */}
    <path 
      d="M11.071 0.929L5.5 6.5L4.429 5.429" 
      stroke="currentColor" 
      strokeWidth="1.8" 
      strokeLinecap="round" 
      strokeLinejoin="round"
      fill="none"
    />
    {/* Second tick (front) */}
    <path 
      d="M15.071 0.929L9.5 6.5L7.429 4.429" 
      stroke="currentColor" 
      strokeWidth="1.8" 
      strokeLinecap="round" 
      strokeLinejoin="round"
      fill="none"
    />
  </svg>
);

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
        // Double tick - bright cyan/teal color for read (distinct from green background)
        return (
          <DoubleTick 
            className="w-[18px] h-[11px]" 
            style={{ color: 'var(--tick-read)' } as React.CSSProperties}
          />
        );
      case 'delivered':
        // Double tick - white color for delivered (recipient online but hasn't read)
        return (
          <DoubleTick 
            className="w-[18px] h-[11px]" 
            style={{ color: 'var(--tick-delivered)' } as React.CSSProperties}
          />
        );
      case 'sent':
        // Single tick - white color for sent (recipient offline)
        return (
          <SingleTick 
            className="w-[14px] h-[11px]" 
            style={{ color: 'var(--tick-sent)' } as React.CSSProperties}
          />
        );
      case 'sending':
        // Clock icon for sending
        return (
          <Clock className="h-3 w-3 text-primary-foreground/50" />
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
          'max-w-[85%] sm:max-w-[75%] md:max-w-[70%] lg:max-w-[65%] px-4 py-2.5',
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
