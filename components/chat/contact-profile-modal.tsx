'use client';

import { X, Phone, Mail, User as UserIcon, Clock, Shield } from 'lucide-react';
import { cn } from '@/lib/utils';
import { formatLastSeen } from '@/lib/format';
import type { User } from '@/lib/types';

interface ContactProfileModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  contact: User;
  isOnline: boolean;
  isBlocked: boolean;
  onBlock: () => void;
  onUnblock: () => void;
}

export function ContactProfileModal({
  open,
  onOpenChange,
  contact,
  isOnline,
  isBlocked,
  onBlock,
  onUnblock,
}: ContactProfileModalProps) {
  if (!open) return null;

  return (
    <div
      className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm"
      onClick={() => onOpenChange(false)}
      role="dialog"
      aria-modal="true"
    >
      <div
        className="relative w-full max-w-md mx-4 glass-panel rounded-2xl shadow-2xl overflow-hidden animate-in fade-in zoom-in-95 duration-200"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header with cover gradient */}
        <div className="relative h-32 bg-gradient-to-br from-primary/40 via-primary/20 to-transparent">
          <button
            type="button"
            onClick={() => onOpenChange(false)}
            className="absolute top-4 right-4 p-2 rounded-full bg-background/50 hover:bg-background/70 transition-colors"
            aria-label="Close"
          >
            <X className="h-5 w-5 text-foreground" />
          </button>
        </div>

        {/* Profile picture */}
        <div className="relative -mt-16 px-6">
          <div className="relative w-32 h-32 mx-auto">
            <div className="w-full h-full rounded-full overflow-hidden ring-4 ring-background shadow-xl">
              <img
                src={contact.avatar || "/placeholder.svg"}
                alt={contact.name}
                className="w-full h-full object-cover"
                crossOrigin="anonymous"
              />
            </div>
            {isOnline && (
              <div className="absolute bottom-2 right-2 w-5 h-5 rounded-full bg-online border-4 border-background" />
            )}
          </div>
        </div>

        {/* Contact info */}
        <div className="px-6 pt-4 pb-6 space-y-6">
          {/* Name and status */}
          <div className="text-center">
            <h2 className="text-2xl font-bold text-foreground">{contact.name}</h2>
            <p className={cn(
              'text-sm mt-1',
              isOnline ? 'text-online' : 'text-muted-foreground'
            )}>
              {isOnline ? 'Online' : contact.lastSeen ? formatLastSeen(contact.lastSeen) : 'Offline'}
            </p>
            {isBlocked && (
              <span className="inline-block mt-2 px-3 py-1 bg-destructive/10 text-destructive text-xs font-medium rounded-full">
                Blocked
              </span>
            )}
          </div>

          {/* Details */}
          <div className="space-y-3">
            {contact.about && (
              <div className="flex items-start gap-3 glass-card p-3 rounded-xl">
                <UserIcon className="h-5 w-5 text-muted-foreground shrink-0 mt-0.5" />
                <div>
                  <p className="text-xs text-muted-foreground">About</p>
                  <p className="text-sm text-foreground">{contact.about}</p>
                </div>
              </div>
            )}

            {contact.email && (
              <div className="flex items-start gap-3 glass-card p-3 rounded-xl">
                <Mail className="h-5 w-5 text-muted-foreground shrink-0 mt-0.5" />
                <div>
                  <p className="text-xs text-muted-foreground">Email</p>
                  <p className="text-sm text-foreground">{contact.email}</p>
                </div>
              </div>
            )}

            {contact.phone && (
              <div className="flex items-start gap-3 glass-card p-3 rounded-xl">
                <Phone className="h-5 w-5 text-muted-foreground shrink-0 mt-0.5" />
                <div>
                  <p className="text-xs text-muted-foreground">Phone</p>
                  <p className="text-sm text-foreground">{contact.phone}</p>
                </div>
              </div>
            )}

            {contact.lastSeen && (
              <div className="flex items-start gap-3 glass-card p-3 rounded-xl">
                <Clock className="h-5 w-5 text-muted-foreground shrink-0 mt-0.5" />
                <div>
                  <p className="text-xs text-muted-foreground">Last seen</p>
                  <p className="text-sm text-foreground">{formatLastSeen(contact.lastSeen)}</p>
                </div>
              </div>
            )}
          </div>

          {/* Block/Unblock button */}
          <div className="pt-2">
            <button
              type="button"
              onClick={isBlocked ? onUnblock : onBlock}
              className={cn(
                'w-full flex items-center justify-center gap-2 py-3 px-4 rounded-xl font-medium transition-colors',
                isBlocked
                  ? 'bg-primary/10 text-primary hover:bg-primary/20'
                  : 'bg-destructive/10 text-destructive hover:bg-destructive/20'
              )}
            >
              <Shield className="h-5 w-5" />
              {isBlocked ? 'Unblock Contact' : 'Block Contact'}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
