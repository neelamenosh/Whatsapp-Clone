'use client';

import { cn } from '@/lib/utils';
import type { Call } from '@/lib/types';
import { formatDistanceToNow, formatDuration } from '@/lib/format';
import { 
  Phone, 
  Video, 
  PhoneIncoming, 
  PhoneOutgoing, 
  PhoneMissed,
  Info
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
        return <PhoneIncoming className="h-4 w-4 text-online" />;
      case 'outgoing':
        return <PhoneOutgoing className="h-4 w-4 text-primary" />;
      case 'missed':
      case 'declined':
        return <PhoneMissed className="h-4 w-4 text-destructive" />;
      default:
        return <Phone className="h-4 w-4 text-muted-foreground" />;
    }
  };

  const getCallText = () => {
    const type = call.type === 'video' ? 'Video' : 'Voice';
    switch (call.status) {
      case 'incoming':
        return `Incoming ${type}`;
      case 'outgoing':
        return `Outgoing ${type}`;
      case 'missed':
        return `Missed ${type}`;
      case 'declined':
        return `Declined ${type}`;
      default:
        return type;
    }
  };

  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        'w-full flex items-center gap-4 p-4 rounded-2xl transition-all duration-300 text-left',
        'hover:bg-card/50 active:scale-[0.99]'
      )}
    >
      {/* Avatar */}
      <div className="relative shrink-0">
        <div className={cn(
          'w-12 h-12 rounded-full overflow-hidden',
          'ring-2 ring-glass-border/30'
        )}>
          <img
            src={participant.avatar || "/placeholder.svg"}
            alt={displayName}
            className="w-full h-full object-cover"
            crossOrigin="anonymous"
          />
        </div>
        {isGroup && (
          <div className="absolute -bottom-1 -right-1 w-5 h-5 rounded-full bg-primary flex items-center justify-center text-[10px] text-primary-foreground font-medium">
            {call.participants.length}
          </div>
        )}
      </div>

      {/* Content */}
      <div className="flex-1 min-w-0">
        <p className={cn(
          'font-semibold truncate',
          call.status === 'missed' ? 'text-destructive' : 'text-foreground'
        )}>
          {displayName}
        </p>
        <div className="flex items-center gap-2 text-sm">
          {getCallIcon()}
          <span className={cn(
            call.status === 'missed' ? 'text-destructive' : 'text-muted-foreground'
          )}>
            {getCallText()}
            {call.duration && ` - ${formatDuration(call.duration)}`}
          </span>
        </div>
      </div>

      {/* Time and action */}
      <div className="flex items-center gap-3 shrink-0">
        <span className="text-sm text-muted-foreground">
          {formatDistanceToNow(call.startedAt)}
        </span>
        <button
          type="button"
          className={cn(
            'w-10 h-10 rounded-full flex items-center justify-center',
            'text-primary hover:bg-primary/10 transition-colors'
          )}
          aria-label={call.type === 'video' ? 'Video call' : 'Voice call'}
        >
          {call.type === 'video' ? (
            <Video className="h-5 w-5" />
          ) : (
            <Phone className="h-5 w-5" />
          )}
        </button>
      </div>
    </button>
  );
}
