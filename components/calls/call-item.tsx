'use client';

import { cn } from '@/lib/utils';
import type { Call } from '@/lib/types';
import { formatDistanceToNow, formatDuration } from '@/lib/format';
import {
  Phone,
  Video,
  PhoneIncoming,
  PhoneOutgoing,
  PhoneMissed
} from 'lucide-react';

interface CallItemProps {
  call: Call;
  onClick?: () => void;
}

export function CallItem({ call, onClick }: CallItemProps) {
  const participant = call.participants[0];
  const isGroup = call.participants.length > 1;
  const displayName = isGroup
    ? call.participants.map((p) => p.name.split(' ')[0]).join(', ')
    : participant.name;

  const getCallIcon = () => {
    switch (call.status) {
      case 'incoming':
        return <PhoneIncoming className="h-3.5 w-3.5 text-primary" />;
      case 'outgoing':
        return <PhoneOutgoing className="h-3.5 w-3.5 text-primary" />;
      case 'missed':
      case 'declined':
        return <PhoneMissed className="h-3.5 w-3.5 text-red-500" />;
      default:
        return <Phone className="h-3.5 w-3.5 text-muted-foreground" />;
    }
  };

  const getCallText = () => {
    const type = call.type === 'video' ? 'Video' : 'Voice';
    switch (call.status) {
      case 'incoming': return `Incoming ${type}`;
      case 'outgoing': return `Outgoing ${type}`;
      case 'missed': return `Missed ${type}`;
      case 'declined': return `Declined ${type}`;
      default: return type;
    }
  };

  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        'w-full flex items-center gap-3 p-3 rounded-2xl transition-all duration-200 text-left mb-1',
        'hover:bg-muted/50 active:scale-[0.98]'
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
        {isGroup && (
          <div className="absolute -bottom-1 -right-1 w-5 h-5 rounded-full bg-primary flex items-center justify-center text-[10px] text-white font-bold">
            {call.participants.length}
          </div>
        )}
      </div>

      {/* Content */}
      <div className="flex-1 min-w-0">
        <div className="flex items-center justify-between mb-0.5">
          <p className={cn(
            'font-bold text-sm truncate',
            call.status === 'missed' ? 'text-red-500' : 'text-foreground'
          )}>
            {displayName}
          </p>
          <span className="text-[10px] text-muted-foreground font-medium shrink-0">
            {formatDistanceToNow(call.startedAt)}
          </span>
        </div>

        <div className="flex items-center justify-between">
          <div className="flex items-center gap-1.5 min-w-0">
            {getCallIcon()}
            <span className={cn(
              'text-xs font-medium truncate',
              call.status === 'missed' ? 'text-red-500/80' : 'text-muted-foreground'
            )}>
              {getCallText()}
              {call.duration && ` • ${formatDuration(call.duration)}`}
            </span>
          </div>

          <div className="flex items-center gap-3 ml-2">
            {call.type === 'video' ? (
              <Video className="w-4 h-4 text-primary" />
            ) : (
              <Phone className="w-4 h-4 text-primary" />
            )}
          </div>
        </div>
      </div>
    </button>
  );
}
