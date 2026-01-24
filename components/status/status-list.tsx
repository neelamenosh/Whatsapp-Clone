'use client';

import { useState } from 'react';
import { cn } from '@/lib/utils';
import { statuses, currentUser } from '@/lib/mock-data';
import { StatusAvatar } from './status-avatar';
import { StatusViewer } from './status-viewer';
import { formatDistanceToNow } from '@/lib/format';
import { Plus, Camera, Pencil, MoreHorizontal } from 'lucide-react';

export function StatusList() {
  const [selectedStatusIndex, setSelectedStatusIndex] = useState<number | null>(null);

  const recentStatuses = statuses.filter(
    (s) => !s.viewedBy.includes(currentUser.id)
  );
  const viewedStatuses = statuses.filter(
    (s) => s.viewedBy.includes(currentUser.id)
  );

  const handleStatusClick = (index: number) => {
    setSelectedStatusIndex(index);
  };

  return (
    <div className="flex flex-col h-full">
      {/* Header */}
      <div className="p-4 pb-2">
        <div className="flex items-center justify-between mb-4">
          <h1 className="text-2xl font-bold text-foreground">Status</h1>
          <button
            type="button"
            className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
            aria-label="More options"
          >
            <MoreHorizontal className="h-5 w-5" />
          </button>
        </div>
      </div>

      <div className="flex-1 overflow-y-auto scrollbar-hide">
        {/* My Status */}
        <div className="px-4 pb-4">
          <button
            type="button"
            className="w-full glass-card p-4 flex items-center gap-4 text-left"
          >
            <div className="relative">
              <div className="w-14 h-14 rounded-full overflow-hidden ring-2 ring-glass-border/30">
                <img
                  src={currentUser.avatar || "/placeholder.svg"}
                  alt="My status"
                  className="w-full h-full object-cover"
                  crossOrigin="anonymous"
                />
              </div>
              <div className="absolute -bottom-1 -right-1 w-6 h-6 rounded-full bg-primary flex items-center justify-center">
                <Plus className="h-4 w-4 text-primary-foreground" />
              </div>
            </div>
            <div className="flex-1">
              <p className="font-semibold text-foreground">My Status</p>
              <p className="text-sm text-muted-foreground">Tap to add status update</p>
            </div>
            <div className="flex gap-2">
              <div className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground">
                <Camera className="h-5 w-5" />
              </div>
              <div className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground">
                <Pencil className="h-5 w-5" />
              </div>
            </div>
          </button>
        </div>

        {/* Status Carousel - Recent Updates */}
        {recentStatuses.length > 0 && (
          <div className="mb-6">
            <p className="px-4 py-2 text-xs font-medium text-muted-foreground uppercase tracking-wider">
              Recent Updates
            </p>
            <div className="flex gap-4 px-4 overflow-x-auto scrollbar-hide pb-2">
              {recentStatuses.map((status, index) => (
                <StatusAvatar
                  key={status.id}
                  status={status}
                  size="lg"
                  hasUnviewed={true}
                  onClick={() => handleStatusClick(index)}
                />
              ))}
            </div>
          </div>
        )}

        {/* Viewed Updates */}
        {viewedStatuses.length > 0 && (
          <div className="mb-6">
            <p className="px-4 py-2 text-xs font-medium text-muted-foreground uppercase tracking-wider">
              Viewed Updates
            </p>
            <div className="flex gap-4 px-4 overflow-x-auto scrollbar-hide pb-2">
              {viewedStatuses.map((status, index) => (
                <StatusAvatar
                  key={status.id}
                  status={status}
                  size="lg"
                  hasUnviewed={false}
                  onClick={() => handleStatusClick(recentStatuses.length + index)}
                />
              ))}
            </div>
          </div>
        )}

        {/* Status List View */}
        <div className="px-4">
          <p className="py-2 text-xs font-medium text-muted-foreground uppercase tracking-wider">
            All Status Updates
          </p>
          {statuses.map((status, index) => (
            <button
              key={status.id}
              type="button"
              onClick={() => handleStatusClick(index)}
              className={cn(
                'w-full flex items-center gap-4 p-3 rounded-2xl transition-all duration-300 text-left',
                'hover:bg-card/50 active:scale-[0.99]'
              )}
            >
              <div className={cn(
                'rounded-full p-[3px]',
                !status.viewedBy.includes(currentUser.id) 
                  ? 'status-ring' 
                  : 'bg-muted/50'
              )}>
                <div className="w-12 h-12 rounded-full overflow-hidden ring-2 ring-background">
                  <img
                    src={status.user.avatar || "/placeholder.svg"}
                    alt={status.user.name}
                    className="w-full h-full object-cover"
                    crossOrigin="anonymous"
                  />
                </div>
              </div>
              <div className="flex-1 min-w-0">
                <p className="font-semibold text-foreground">{status.user.name}</p>
                <p className="text-sm text-muted-foreground">
                  {formatDistanceToNow(status.createdAt)} ago
                </p>
              </div>
            </button>
          ))}
        </div>
      </div>

      {/* Status Viewer Modal */}
      {selectedStatusIndex !== null && (
        <StatusViewer
          statuses={statuses}
          initialIndex={selectedStatusIndex}
          onClose={() => setSelectedStatusIndex(null)}
        />
      )}
    </div>
  );
}
