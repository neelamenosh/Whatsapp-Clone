'use client';

import { cn } from '@/lib/utils';
import type { Status } from '@/lib/types';

interface StatusAvatarProps {
  status: Status;
  size?: 'sm' | 'md' | 'lg';
  hasUnviewed?: boolean;
  onClick?: () => void;
}

export function StatusAvatar({ 
  status, 
  size = 'md', 
  hasUnviewed = false,
  onClick 
}: StatusAvatarProps) {
  const sizeClasses = {
    sm: 'w-12 h-12',
    md: 'w-16 h-16',
    lg: 'w-20 h-20',
  };

  const ringSize = {
    sm: 'p-0.5',
    md: 'p-[3px]',
    lg: 'p-1',
  };

  return (
    <button
      type="button"
      onClick={onClick}
      className="flex flex-col items-center gap-2 group"
    >
      <div className={cn(
        'rounded-full',
        ringSize[size],
        hasUnviewed 
          ? 'status-ring' 
          : 'bg-muted/50'
      )}>
        <div className={cn(
          sizeClasses[size],
          'rounded-full overflow-hidden',
          'ring-2 ring-background',
          'transition-transform duration-300 group-hover:scale-105'
        )}>
          <img
            src={status.user.avatar || "/placeholder.svg"}
            alt={status.user.name}
            className="w-full h-full object-cover"
            crossOrigin="anonymous"
          />
        </div>
      </div>
      <span className="text-xs text-muted-foreground truncate max-w-[72px]">
        {status.user.name.split(' ')[0]}
      </span>
    </button>
  );
}
