'use client';

import { cn } from '@/lib/utils';

interface TypingIndicatorProps {
  userName?: string;
}

export function TypingIndicator({ userName }: TypingIndicatorProps) {
  return (
    <div className="flex justify-start mb-2">
      <div className="message-bubble-received px-4 py-3 flex items-center gap-1">
        <div className="typing-dot w-2 h-2 rounded-full bg-muted-foreground/60" />
        <div className="typing-dot w-2 h-2 rounded-full bg-muted-foreground/60" />
        <div className="typing-dot w-2 h-2 rounded-full bg-muted-foreground/60" />
        {userName && (
          <span className="ml-2 text-xs text-muted-foreground">
            {userName} is typing
          </span>
        )}
      </div>
    </div>
  );
}
