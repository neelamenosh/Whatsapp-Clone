'use client';

import { useState, useRef, useEffect } from 'react';
import { cn } from '@/lib/utils';
import type { Chat } from '@/lib/types';
import { formatDistanceToNow } from '@/lib/format';
import { useSettings } from '@/components/settings-provider';
import { getCurrentUser } from '@/lib/auth-store';
import { isSupabaseConfigured } from '@/lib/supabase/client';
import * as supabaseMessages from '@/lib/supabase/messages';
import { getConsistentChatId } from '@/lib/live-chat';
import { 
  Check, 
  CheckCheck, 
  Pin, 
  VolumeX, 
  ExternalLink, 
  CheckCircle, 
  Archive, 
  Bell, 
  BellOff,
  Info,
  Download,
  Trash2,
  XCircle
} from 'lucide-react';

interface ChatListItemProps {
  chat: Chat;
  isSelected: boolean;
  onClick: () => void;
  onChatUpdate?: (updatedChat: Chat) => void;
  onChatDelete?: (chatId: string) => void;
}

export function ChatListItem({ chat, isSelected, onClick, onChatUpdate, onChatDelete }: ChatListItemProps) {
  const { settings, updateSettings } = useSettings();
  const [showContextMenu, setShowContextMenu] = useState(false);
  const [contextMenuPosition, setContextMenuPosition] = useState({ x: 0, y: 0 });
  const [showMuteSubmenu, setShowMuteSubmenu] = useState(false);
  const [showClearConfirm, setShowClearConfirm] = useState(false);
  const [showDeleteConfirm, setShowDeleteConfirm] = useState(false);
  const [isClearing, setIsClearing] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);
  
  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;
  const isBlocked = !isGroup && settings.privacy.blockedUserIds.includes(participant.id);
  const loggedInUser = getCurrentUser();

  // Close menu when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        setShowContextMenu(false);
        setShowMuteSubmenu(false);
      }
    };

    if (showContextMenu) {
      document.addEventListener('mousedown', handleClickOutside);
    }

    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, [showContextMenu]);

  // Handle context menu (right-click or two-finger tap)
  const handleContextMenu = (e: React.MouseEvent | React.TouchEvent) => {
    e.preventDefault();
    e.stopPropagation();
    
    let x: number, y: number;
    
    if ('touches' in e) {
      // Touch event
      x = e.touches[0]?.clientX || 0;
      y = e.touches[0]?.clientY || 0;
    } else {
      // Mouse event
      x = e.clientX;
      y = e.clientY;
    }

    // Adjust position to stay within viewport
    const menuWidth = 220;
    const menuHeight = 400;
    const viewportWidth = window.innerWidth;
    const viewportHeight = window.innerHeight;

    if (x + menuWidth > viewportWidth) {
      x = viewportWidth - menuWidth - 10;
    }
    if (y + menuHeight > viewportHeight) {
      y = viewportHeight - menuHeight - 10;
    }

    setContextMenuPosition({ x, y });
    setShowContextMenu(true);
  };

  const getStatusIcon = () => {
    if (!chat.lastMessage || chat.lastMessage.senderId !== 'current-user') return null;
    
    switch (chat.lastMessage.status) {
      case 'read':
        return settings.privacy.readReceipts
          ? <CheckCheck className="h-4 w-4 text-primary" />
          : <CheckCheck className="h-4 w-4 text-muted-foreground" />;
      case 'delivered':
        return <CheckCheck className="h-4 w-4 text-muted-foreground" />;
      case 'sent':
        return <Check className="h-4 w-4 text-muted-foreground" />;
      default:
        return null;
    }
  };

  // Menu actions
  const handleMarkAsRead = () => {
    if (onChatUpdate) {
      onChatUpdate({ ...chat, unreadCount: 0 });
    }
    setShowContextMenu(false);
  };

  const handleArchive = () => {
    // Archive functionality - could be stored in settings or Supabase
    console.log('Archive chat:', chat.id);
    setShowContextMenu(false);
  };

  const handleTogglePin = () => {
    if (onChatUpdate) {
      onChatUpdate({ ...chat, isPinned: !chat.isPinned });
    }
    setShowContextMenu(false);
  };

  const handleToggleMute = (duration: number | null) => {
    if (onChatUpdate) {
      onChatUpdate({ ...chat, isMuted: duration !== null });
    }
    setShowContextMenu(false);
    setShowMuteSubmenu(false);
  };

  const handleContactInfo = () => {
    // This will be handled by the parent - navigate to contact profile
    onClick();
    setShowContextMenu(false);
  };

  const handleExportChat = () => {
    // Export chat as text file
    const messages = chat.lastMessage ? [chat.lastMessage] : [];
    const content = `Chat with ${displayName}\n\n${messages.map(m => 
      `[${m.timestamp.toLocaleString()}] ${m.senderId === loggedInUser?.id ? 'You' : displayName}: ${m.content}`
    ).join('\n')}`;
    
    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `chat-${displayName.replace(/\s+/g, '-').toLowerCase()}.txt`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    
    setShowContextMenu(false);
  };

  const handleClearChat = async () => {
    if (!loggedInUser) return;
    
    setIsClearing(true);
    
    try {
      if (isSupabaseConfigured()) {
        const consistentChatId = getConsistentChatId(loggedInUser.id, participant.id);
        const result = await supabaseMessages.clearChat(loggedInUser.id, participant.id);
        if (result.error) {
          console.error('Failed to clear chat:', result.error);
          alert('Failed to clear chat. Please try again.');
          return;
        }
        // Clear from localStorage
        localStorage.removeItem(`whatsapp_messages_${consistentChatId}`);
      }
      
      if (onChatUpdate) {
        onChatUpdate({ ...chat, lastMessage: undefined, unreadCount: 0 });
      }
    } catch (err) {
      console.error('Error clearing chat:', err);
    } finally {
      setIsClearing(false);
      setShowClearConfirm(false);
      setShowContextMenu(false);
    }
  };

  const handleDeleteChat = async () => {
    if (!loggedInUser) return;
    
    try {
      // Clear messages first
      if (isSupabaseConfigured()) {
        const consistentChatId = getConsistentChatId(loggedInUser.id, participant.id);
        await supabaseMessages.clearChat(loggedInUser.id, participant.id);
        localStorage.removeItem(`whatsapp_messages_${consistentChatId}`);
      }
      
      if (onChatDelete) {
        onChatDelete(chat.id);
      }
    } catch (err) {
      console.error('Error deleting chat:', err);
    } finally {
      setShowDeleteConfirm(false);
      setShowContextMenu(false);
    }
  };

  return (
    <>
      <button
        type="button"
        onClick={onClick}
        onContextMenu={handleContextMenu}
        className={cn(
          'w-full flex items-center gap-3 p-4 rounded-2xl transition-all duration-300 text-left',
          'hover:scale-[1.01] active:scale-[0.99]',
          isSelected 
            ? 'glass-card bg-primary/10' 
            : 'hover:bg-card/50'
        )}
      >
      {/* Avatar with online indicator */}
      <div className="relative shrink-0">
        <div className={cn(
          'w-14 h-14 rounded-full overflow-hidden',
          'ring-2 ring-glass-border/50',
          'shadow-lg'
        )}>
          <img
            src={participant.avatar || "/placeholder.svg"}
            alt={displayName}
            className="w-full h-full object-cover"
            crossOrigin="anonymous"
          />
        </div>
        {participant.status === 'online' && (
          <div className="absolute bottom-0 right-0 w-4 h-4 rounded-full bg-online border-2 border-background online-pulse" />
        )}
        {isGroup && (
          <div className="absolute -bottom-1 -right-1 w-5 h-5 rounded-full bg-primary flex items-center justify-center text-[10px] text-primary-foreground font-medium">
            {chat.participants.length}
          </div>
        )}
      </div>

      {/* Content */}
      <div className="flex-1 min-w-0">
        <div className="flex items-center justify-between mb-1">
          <div className="flex items-center gap-2">
            <span className="font-semibold text-foreground truncate">
              {displayName}
            </span>
            {chat.isPinned && (
              <Pin className="h-3 w-3 text-muted-foreground fill-muted-foreground" />
            )}
            {chat.isMuted && (
              <VolumeX className="h-3 w-3 text-muted-foreground" />
            )}
          </div>
          <span className={cn(
            'text-xs shrink-0',
            chat.unreadCount > 0 ? 'text-primary font-medium' : 'text-muted-foreground'
          )}>
            {chat.lastMessage && formatDistanceToNow(chat.lastMessage.timestamp)}
          </span>
        </div>
        
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-1 min-w-0 flex-1">
            {getStatusIcon()}
            <p className={cn(
              'text-sm truncate',
              chat.unreadCount > 0 ? 'text-foreground font-medium' : 'text-muted-foreground'
            )}>
              {isBlocked ? 'Blocked' : (chat.lastMessage?.content || 'No messages yet')}
            </p>
          </div>
          
          {chat.unreadCount > 0 && (
            <div className="ml-2 min-w-[20px] h-5 px-1.5 rounded-full bg-primary flex items-center justify-center badge-glow">
              <span className="text-xs font-semibold text-primary-foreground">
                {chat.unreadCount}
              </span>
            </div>
          )}
        </div>
      </div>
    </button>

    {/* Context Menu */}
    {showContextMenu && (
      <div
        ref={menuRef}
        className="fixed z-50 w-56 glass-panel rounded-xl shadow-2xl py-2 animate-in fade-in zoom-in-95 duration-150"
        style={{ left: contextMenuPosition.x, top: contextMenuPosition.y }}
      >
        {/* Open in New Window */}
        <button
          type="button"
          onClick={() => {
            window.open(window.location.href, '_blank');
            setShowContextMenu(false);
          }}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
        >
          <ExternalLink className="h-5 w-5 text-muted-foreground" />
          <span className="text-sm text-foreground">Open in New Window</span>
        </button>

        {/* Mark as Read */}
        {chat.unreadCount > 0 && (
          <button
            type="button"
            onClick={handleMarkAsRead}
            className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
          >
            <CheckCircle className="h-5 w-5 text-muted-foreground" />
            <span className="text-sm text-foreground">Mark as read</span>
          </button>
        )}

        {/* Archive */}
        <button
          type="button"
          onClick={handleArchive}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
        >
          <Archive className="h-5 w-5 text-muted-foreground" />
          <span className="text-sm text-foreground">Archive</span>
        </button>

        {/* Pin */}
        <button
          type="button"
          onClick={handleTogglePin}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
        >
          <Pin className={cn("h-5 w-5", chat.isPinned ? "text-primary fill-primary" : "text-muted-foreground")} />
          <span className="text-sm text-foreground">{chat.isPinned ? 'Unpin' : 'Pin'}</span>
        </button>

        {/* Mute */}
        <div className="relative">
          <button
            type="button"
            onClick={() => setShowMuteSubmenu(!showMuteSubmenu)}
            className="w-full px-4 py-2.5 flex items-center justify-between hover:bg-muted/50 transition-colors text-left"
          >
            <div className="flex items-center gap-3">
              {chat.isMuted ? (
                <BellOff className="h-5 w-5 text-muted-foreground" />
              ) : (
                <Bell className="h-5 w-5 text-muted-foreground" />
              )}
              <span className="text-sm text-foreground">{chat.isMuted ? 'Unmute' : 'Mute'}</span>
            </div>
            <span className="text-muted-foreground">›</span>
          </button>

          {/* Mute Submenu */}
          {showMuteSubmenu && (
            <div className="absolute left-full top-0 ml-1 w-48 glass-panel rounded-xl shadow-2xl py-2 animate-in fade-in slide-in-from-left-2 duration-150">
              {chat.isMuted ? (
                <button
                  type="button"
                  onClick={() => handleToggleMute(null)}
                  className="w-full px-4 py-2.5 hover:bg-muted/50 transition-colors text-left text-sm text-foreground"
                >
                  Unmute
                </button>
              ) : (
                <>
                  <button
                    type="button"
                    onClick={() => handleToggleMute(8 * 60 * 60 * 1000)}
                    className="w-full px-4 py-2.5 hover:bg-muted/50 transition-colors text-left text-sm text-foreground"
                  >
                    8 hours
                  </button>
                  <button
                    type="button"
                    onClick={() => handleToggleMute(7 * 24 * 60 * 60 * 1000)}
                    className="w-full px-4 py-2.5 hover:bg-muted/50 transition-colors text-left text-sm text-foreground"
                  >
                    1 week
                  </button>
                  <button
                    type="button"
                    onClick={() => handleToggleMute(-1)}
                    className="w-full px-4 py-2.5 hover:bg-muted/50 transition-colors text-left text-sm text-foreground"
                  >
                    Always
                  </button>
                </>
              )}
            </div>
          )}
        </div>

        <div className="h-px bg-border my-2" />

        {/* Contact Info */}
        <button
          type="button"
          onClick={handleContactInfo}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
        >
          <Info className="h-5 w-5 text-muted-foreground" />
          <span className="text-sm text-foreground">Contact info</span>
        </button>

        {/* Export Chat */}
        <button
          type="button"
          onClick={handleExportChat}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
        >
          <Download className="h-5 w-5 text-muted-foreground" />
          <span className="text-sm text-foreground">Export chat</span>
        </button>

        {/* Clear Chat */}
        <button
          type="button"
          onClick={() => {
            setShowContextMenu(false);
            setShowClearConfirm(true);
          }}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
        >
          <XCircle className="h-5 w-5 text-muted-foreground" />
          <span className="text-sm text-foreground">Clear chat</span>
        </button>

        {/* Delete Chat */}
        <button
          type="button"
          onClick={() => {
            setShowContextMenu(false);
            setShowDeleteConfirm(true);
          }}
          className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left text-destructive"
        >
          <Trash2 className="h-5 w-5" />
          <span className="text-sm">Delete chat</span>
        </button>
      </div>
    )}

    {/* Clear Chat Confirmation Modal */}
    {showClearConfirm && (
      <div
        className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm"
        onClick={() => setShowClearConfirm(false)}
      >
        <div
          className="w-full max-w-sm mx-4 glass-panel rounded-2xl shadow-2xl overflow-hidden animate-in fade-in zoom-in-95 duration-200"
          onClick={(e) => e.stopPropagation()}
        >
          <div className="p-6 text-center">
            <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-destructive/10 flex items-center justify-center">
              <XCircle className="h-8 w-8 text-destructive" />
            </div>
            <h3 className="text-lg font-semibold text-foreground mb-2">Clear Chat?</h3>
            <p className="text-sm text-muted-foreground mb-6">
              Are you sure you want to clear all messages with <span className="font-medium text-foreground">{displayName}</span>?
            </p>
            <div className="flex gap-3">
              <button
                type="button"
                onClick={() => setShowClearConfirm(false)}
                disabled={isClearing}
                className="flex-1 px-4 py-2.5 bg-muted hover:bg-muted/80 text-foreground font-medium rounded-xl transition-colors disabled:opacity-50"
              >
                Cancel
              </button>
              <button
                type="button"
                onClick={handleClearChat}
                disabled={isClearing}
                className="flex-1 px-4 py-2.5 bg-destructive hover:bg-destructive/90 text-destructive-foreground font-medium rounded-xl transition-colors disabled:opacity-50"
              >
                {isClearing ? 'Clearing...' : 'Clear'}
              </button>
            </div>
          </div>
        </div>
      </div>
    )}

    {/* Delete Chat Confirmation Modal */}
    {showDeleteConfirm && (
      <div
        className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm"
        onClick={() => setShowDeleteConfirm(false)}
      >
        <div
          className="w-full max-w-sm mx-4 glass-panel rounded-2xl shadow-2xl overflow-hidden animate-in fade-in zoom-in-95 duration-200"
          onClick={(e) => e.stopPropagation()}
        >
          <div className="p-6 text-center">
            <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-destructive/10 flex items-center justify-center">
              <Trash2 className="h-8 w-8 text-destructive" />
            </div>
            <h3 className="text-lg font-semibold text-foreground mb-2">Delete Chat?</h3>
            <p className="text-sm text-muted-foreground mb-6">
              This will delete all messages and remove the chat with <span className="font-medium text-foreground">{displayName}</span> from your list.
            </p>
            <div className="flex gap-3">
              <button
                type="button"
                onClick={() => setShowDeleteConfirm(false)}
                className="flex-1 px-4 py-2.5 bg-muted hover:bg-muted/80 text-foreground font-medium rounded-xl transition-colors"
              >
                Cancel
              </button>
              <button
                type="button"
                onClick={handleDeleteChat}
                className="flex-1 px-4 py-2.5 bg-destructive hover:bg-destructive/90 text-destructive-foreground font-medium rounded-xl transition-colors"
              >
                Delete
              </button>
            </div>
          </div>
        </div>
      </div>
    )}
  </>
  );
}
