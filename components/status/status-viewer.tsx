'use client';

import { useState, useEffect, useCallback } from 'react';
import { cn } from '@/lib/utils';
import type { Status } from '@/lib/types';
import { formatDistanceToNow } from '@/lib/format';
import { X, ChevronLeft, ChevronRight, Send } from 'lucide-react';

interface StatusViewerProps {
  statuses: Status[];
  initialIndex: number;
  onClose: () => void;
}

export function StatusViewer({ statuses, initialIndex, onClose }: StatusViewerProps) {
  const [currentIndex, setCurrentIndex] = useState(initialIndex);
  const [progress, setProgress] = useState(0);
  const [isPaused, setIsPaused] = useState(false);
  const [replyText, setReplyText] = useState('');

  const currentStatus = statuses[currentIndex];
  const duration = 5000; // 5 seconds per status

  const goToNext = useCallback(() => {
    if (currentIndex < statuses.length - 1) {
      setCurrentIndex((prev) => prev + 1);
      setProgress(0);
    } else {
      onClose();
    }
  }, [currentIndex, statuses.length, onClose]);

  const goToPrev = useCallback(() => {
    if (currentIndex > 0) {
      setCurrentIndex((prev) => prev - 1);
      setProgress(0);
    }
  }, [currentIndex]);

  useEffect(() => {
    if (isPaused) return;

    const interval = setInterval(() => {
      setProgress((prev) => {
        if (prev >= 100) {
          goToNext();
          return 0;
        }
        return prev + (100 / (duration / 50));
      });
    }, 50);

    return () => clearInterval(interval);
  }, [isPaused, goToNext, duration]);

  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'Escape') onClose();
      if (e.key === 'ArrowLeft') goToPrev();
      if (e.key === 'ArrowRight') goToNext();
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [onClose, goToPrev, goToNext]);

  return (
    <div className="fixed inset-0 z-50 bg-background/95 backdrop-blur-xl animate-in fade-in-0 duration-300">
      {/* Progress bars */}
      <div className="absolute top-0 left-0 right-0 flex gap-1 p-3 z-10">
        {statuses.map((_, index) => (
          <div
            key={statuses[index].id}
            className="flex-1 h-1 rounded-full bg-foreground/20 overflow-hidden"
          >
            <div
              className="h-full bg-foreground/80 transition-all duration-50 ease-linear"
              style={{
                width: index < currentIndex 
                  ? '100%' 
                  : index === currentIndex 
                    ? `${progress}%` 
                    : '0%'
              }}
            />
          </div>
        ))}
      </div>

      {/* Header */}
      <div className="absolute top-8 left-0 right-0 flex items-center justify-between px-4 z-10">
        <div className="flex items-center gap-3">
          <div className="w-10 h-10 rounded-full overflow-hidden ring-2 ring-foreground/20">
            <img
              src={currentStatus.user.avatar || "/placeholder.svg"}
              alt={currentStatus.user.name}
              className="w-full h-full object-cover"
              crossOrigin="anonymous"
            />
          </div>
          <div>
            <p className="font-semibold text-foreground">{currentStatus.user.name}</p>
            <p className="text-xs text-muted-foreground">
              {formatDistanceToNow(currentStatus.createdAt)} ago
            </p>
          </div>
        </div>
        <button
          type="button"
          onClick={onClose}
          className="p-2 rounded-full hover:bg-foreground/10 transition-colors"
          aria-label="Close"
        >
          <X className="h-6 w-6 text-foreground" />
        </button>
      </div>

      {/* Content */}
      <div 
        className="absolute inset-0 flex items-center justify-center"
        onMouseDown={() => setIsPaused(true)}
        onMouseUp={() => setIsPaused(false)}
        onTouchStart={() => setIsPaused(true)}
        onTouchEnd={() => setIsPaused(false)}
      >
        {currentStatus.mediaUrl ? (
          <img
            src={currentStatus.mediaUrl || "/placeholder.svg"}
            alt="Status"
            className="max-w-full max-h-full object-contain"
            crossOrigin="anonymous"
          />
        ) : (
          <div 
            className="w-full h-full flex items-center justify-center p-8"
            style={{ backgroundColor: currentStatus.backgroundColor }}
          >
            <p className="text-2xl font-semibold text-white text-center">
              {currentStatus.content}
            </p>
          </div>
        )}
      </div>

      {/* Navigation areas */}
      <button
        type="button"
        onClick={goToPrev}
        className="absolute left-0 top-20 bottom-24 w-1/3 z-10"
        aria-label="Previous status"
      >
        <span className="sr-only">Previous</span>
      </button>
      <button
        type="button"
        onClick={goToNext}
        className="absolute right-0 top-20 bottom-24 w-1/3 z-10"
        aria-label="Next status"
      >
        <span className="sr-only">Next</span>
      </button>

      {/* Navigation buttons (visible on hover) */}
      {currentIndex > 0 && (
        <button
          type="button"
          onClick={goToPrev}
          className="absolute left-4 top-1/2 -translate-y-1/2 p-3 rounded-full glass-panel opacity-0 hover:opacity-100 transition-opacity z-20"
          aria-label="Previous"
        >
          <ChevronLeft className="h-6 w-6 text-foreground" />
        </button>
      )}
      {currentIndex < statuses.length - 1 && (
        <button
          type="button"
          onClick={goToNext}
          className="absolute right-4 top-1/2 -translate-y-1/2 p-3 rounded-full glass-panel opacity-0 hover:opacity-100 transition-opacity z-20"
          aria-label="Next"
        >
          <ChevronRight className="h-6 w-6 text-foreground" />
        </button>
      )}

      {/* Reply input */}
      <div className="absolute bottom-0 left-0 right-0 p-4 z-10">
        <div className="flex items-center gap-3">
          <input
            type="text"
            placeholder="Reply..."
            value={replyText}
            onChange={(e) => setReplyText(e.target.value)}
            className="flex-1 glass-input px-5 py-3 text-foreground placeholder:text-muted-foreground"
          />
          {replyText.trim() && (
            <button
              type="button"
              className={cn(
                'w-12 h-12 rounded-full bg-primary text-primary-foreground',
                'flex items-center justify-center',
                'transition-all duration-200 hover:scale-105 active:scale-95'
              )}
              aria-label="Send reply"
            >
              <Send className="h-5 w-5" />
            </button>
          )}
        </div>
      </div>
    </div>
  );
}
