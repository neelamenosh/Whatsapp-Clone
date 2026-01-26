'use client';

import { useState, useRef, useEffect, useCallback } from 'react';
import { cn } from '@/lib/utils';
import type { Chat, Message } from '@/lib/types';
import { currentUser } from '@/lib/mock-data';
import { getCurrentUser } from '@/lib/auth-store';
import { formatLastSeen } from '@/lib/format';
import { useSettings } from '@/components/settings-provider';
import { Virtuoso } from 'react-virtuoso';
import { getLiveChatService, getConsistentChatId } from '@/lib/live-chat';
import { MessageBubble } from './message-bubble';
import { TypingIndicator } from './typing-indicator';
import { 
  ArrowLeft, 
  Phone, 
  Video, 
  MoreVertical, 
  Smile, 
  Paperclip, 
  Mic, 
  Send,
  Camera
} from 'lucide-react';

interface ConversationViewProps {
  chat: Chat;
  onBack: () => void;
}

export function ConversationView({ chat, onBack }: ConversationViewProps) {
  const [messages, setMessages] = useState<Message[]>([]);
  const [inputValue, setInputValue] = useState('');
  const [isTyping, setIsTyping] = useState(false);
  const [isOnline, setIsOnline] = useState(false);
  const inputRef = useRef<HTMLInputElement>(null);
  const typingTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const { settings } = useSettings();
  
  const loggedInUser = getCurrentUser();
  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;
  const isBlocked = !isGroup && settings.privacy.blockedUserIds.includes(participant.id);
  
  // Get consistent chat ID for message storage
  const consistentChatId = !isGroup && loggedInUser 
    ? getConsistentChatId(loggedInUser.id, participant.id) 
    : chat.id;

  // Load messages from localStorage on mount
  useEffect(() => {
    const liveChatService = getLiveChatService();
    // Use consistent chat ID for loading messages
    const storedMessages = liveChatService.getMessages(consistentChatId);
    setMessages(storedMessages);
    
    // Check if participant is online
    setIsOnline(liveChatService.isUserOnline(participant.id));
  }, [consistentChatId, participant.id]);

  // Listen for incoming messages
  useEffect(() => {
    const liveChatService = getLiveChatService();
    
    const unsubMessage = liveChatService.onMessage((chatId, message) => {
      // Check if this message is for the current conversation
      if (chatId !== consistentChatId) return;
      
      setMessages((prev) => {
        // Check for duplicates
        if (prev.some(m => m.id === message.id)) {
          return prev;
        }
        return [...prev, message];
      });
    });

    return () => {
      unsubMessage();
    };
  }, [consistentChatId]);

  // Listen for typing indicator
  useEffect(() => {
    const liveChatService = getLiveChatService();
    
    const unsubTyping = liveChatService.onTyping((chatId, userId, typing) => {
      if (chatId !== consistentChatId) return;
      if (userId === participant.id) {
        setIsTyping(typing);
      }
    });

    return () => {
      unsubTyping();
    };
  }, [consistentChatId, participant.id]);

  // Listen for online status changes
  useEffect(() => {
    const liveChatService = getLiveChatService();
    
    const unsubOnline = liveChatService.onOnlineStatus((userId, online) => {
      if (userId === participant.id) {
        setIsOnline(online);
      }
    });

    // Periodically check online status
    const interval = setInterval(() => {
      setIsOnline(liveChatService.isUserOnline(participant.id));
    }, 5000);

    return () => {
      unsubOnline();
      clearInterval(interval);
    };
  }, [participant.id]);

  // Handle typing indicator emission
  const handleInputChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setInputValue(e.target.value);
    
    const liveChatService = getLiveChatService();
    liveChatService.sendTyping(consistentChatId, true);
    
    // Clear existing timeout
    if (typingTimeoutRef.current) {
      clearTimeout(typingTimeoutRef.current);
    }
    
    // Stop typing indicator after 2 seconds of no input
    typingTimeoutRef.current = setTimeout(() => {
      liveChatService.sendTyping(consistentChatId, false);
    }, 2000);
  }, [consistentChatId]);

  const listData = messages;

  const handleSend = () => {
    if (isBlocked) return;
    if (!inputValue.trim()) return;
    if (!loggedInUser) return;

    const ttlSeconds = settings.privacy.disappearingMessagesSeconds;
    const expiresAt = ttlSeconds ? new Date(Date.now() + ttlSeconds * 1000) : undefined;

    // Generate client-side ID
    const messageId = `m-${Date.now()}-${Math.random().toString(16).slice(2)}`;

    const newMessage: Message = {
      id: messageId,
      senderId: loggedInUser.id,
      content: inputValue,
      timestamp: new Date(),
      status: 'sending',
      type: 'text',
      expiresAt,
    };

    // Check if this is the first message (new chat for recipient)
    const isFirstMessage = messages.length === 0;

    // Add to local state immediately
    setMessages((prev) => [...prev, newMessage]);
    setInputValue('');
    
    // Stop typing indicator
    const liveChatService = getLiveChatService();
    liveChatService.sendTyping(consistentChatId, false);
    
    // Send via live chat service with recipient ID for proper routing
    liveChatService.sendMessage(consistentChatId, newMessage, participant.id);

    // If first message, notify recipient about new chat
    if (isFirstMessage && !isGroup) {
      liveChatService.notifyNewChat(
        { ...chat, id: consistentChatId, lastMessage: newMessage },
        participant.id
      );
    }

    // Update message status
    setTimeout(() => {
      setMessages((prev) =>
        prev.map((m) =>
          m.id === newMessage.id ? { ...m, status: 'sent' } : m
        )
      );
    }, 300);

    setTimeout(() => {
      setMessages((prev) =>
        prev.map((m) =>
          m.id === newMessage.id ? { ...m, status: 'delivered' } : m
        )
      );
    }, 800);

    setTimeout(() => {
      if (settings.privacy.readReceipts) {
        setMessages((prev) =>
          prev.map((m) =>
            m.id === newMessage.id ? { ...m, status: 'read' } : m
          )
        );
      }
    }, 1500);
  };

  const getStatusText = () => {
    if (isGroup) {
      return `${chat.participants.length} members`;
    }
    if (isBlocked) {
      return 'blocked';
    }
    if (isOnline) {
      return 'online';
    }
    if (participant.lastSeen) {
      return formatLastSeen(participant.lastSeen);
    }
    return 'offline';
  };

  return (
    <div className="flex flex-col h-full page-transition">
      {/* Header */}
      <div className="glass-panel px-4 py-3 flex items-center gap-3 z-10">
        <button
          type="button"
          onClick={onBack}
          className="p-2 -ml-2 rounded-full hover:bg-muted/50 transition-colors"
          aria-label="Go back"
        >
          <ArrowLeft className="h-5 w-5 text-foreground" />
        </button>

        <div className="flex items-center gap-3 flex-1 min-w-0">
          <div className="relative shrink-0">
            <div className="w-10 h-10 rounded-full overflow-hidden ring-2 ring-glass-border/30">
              <img
                src={participant.avatar || "/placeholder.svg"}
                alt={displayName}
                className="w-full h-full object-cover"
                crossOrigin="anonymous"
              />
            </div>
            {isOnline && !isGroup && (
              <div className="absolute bottom-0 right-0 w-3 h-3 rounded-full bg-online border-2 border-background" />
            )}
          </div>

          <div className="min-w-0">
            <h2 className="font-semibold text-foreground truncate">{displayName}</h2>
            <p className={cn(
              'text-xs',
              isOnline ? 'text-online' : 'text-muted-foreground'
            )}>
              {getStatusText()}
            </p>
          </div>
        </div>

        <div className="flex items-center gap-1">
          <button
            type="button"
            className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
            aria-label="Video call"
          >
            <Video className="h-5 w-5" />
          </button>
          <button
            type="button"
            className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
            aria-label="Voice call"
          >
            <Phone className="h-5 w-5" />
          </button>
          <button
            type="button"
            className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
            aria-label="More options"
          >
            <MoreVertical className="h-5 w-5" />
          </button>
        </div>
      </div>

      {/* Messages */}
      <div className="flex-1 overflow-hidden">
        <Virtuoso
          className="h-full px-4 py-4 scrollbar-hide"
          data={listData}
          followOutput="smooth"
          itemContent={(_, message) => (
            <MessageBubble
              key={message.id}
              message={message}
              isOwn={message.senderId === loggedInUser?.id}
            />
          )}
          components={{
            Header: () => (
              <div className="space-y-4">
                {isBlocked && (
                  <div className="flex justify-center">
                    <div className="glass-card px-4 py-2 text-xs text-muted-foreground">
                      You blocked this contact. Unblock them in Settings to send messages.
                    </div>
                  </div>
                )}
                <div className="flex justify-center">
                  <div className="glass-card px-4 py-1.5 text-xs text-muted-foreground">
                    Today
                  </div>
                </div>
              </div>
            ),
            Footer: () => (
              <div className="pb-2">
                {isTyping ? <TypingIndicator /> : null}
              </div>
            ),
          }}
        />
      </div>

      {/* Input */}
      <div className="glass-panel px-4 py-3">
        <div className="flex items-center gap-2">
          <button
            type="button"
            className="p-2 rounded-full hover:bg-muted/50 transition-colors text-muted-foreground hover:text-foreground"
            aria-label="Add emoji"
          >
            <Smile className="h-6 w-6" />
          </button>

          <div className="flex-1 relative">
            <input
              ref={inputRef}
              type="text"
              placeholder="Message..."
              value={inputValue}
              onChange={handleInputChange}
              onKeyDown={(e) => e.key === 'Enter' && handleSend()}
              disabled={isBlocked}
              className="w-full glass-input px-4 py-3 pr-24 text-sm text-foreground placeholder:text-muted-foreground disabled:opacity-60"
            />
            <div className="absolute right-2 top-1/2 -translate-y-1/2 flex items-center gap-1">
              <button
                type="button"
                className="p-2 rounded-full hover:bg-muted/50 transition-colors text-muted-foreground hover:text-foreground"
                aria-label="Attach file"
              >
                <Paperclip className="h-5 w-5" />
              </button>
              <button
                type="button"
                className="p-2 rounded-full hover:bg-muted/50 transition-colors text-muted-foreground hover:text-foreground"
                aria-label="Camera"
              >
                <Camera className="h-5 w-5" />
              </button>
            </div>
          </div>

          {inputValue.trim() ? (
            <button
              type="button"
              onClick={handleSend}
              className={cn(
                'w-12 h-12 rounded-full bg-primary text-primary-foreground',
                'flex items-center justify-center',
                'shadow-lg shadow-primary/30',
                'transition-all duration-200 hover:scale-105 active:scale-95'
              )}
              aria-label="Send message"
            >
              <Send className="h-5 w-5" />
            </button>
          ) : (
            <button
              type="button"
              className={cn(
                'w-12 h-12 rounded-full bg-primary text-primary-foreground',
                'flex items-center justify-center',
                'shadow-lg shadow-primary/30',
                'transition-all duration-200 hover:scale-105 active:scale-95'
              )}
              aria-label="Voice message"
            >
              <Mic className="h-5 w-5" />
            </button>
          )}
        </div>
      </div>
    </div>
  );
}