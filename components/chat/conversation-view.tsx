'use client';

import { useState, useRef, useEffect } from 'react';
import { cn } from '@/lib/utils';
import type { Chat, Message } from '@/lib/types';
import { getMessages, currentUser } from '@/lib/mock-data';
import { formatLastSeen } from '@/lib/format';
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
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;

  useEffect(() => {
    setMessages(getMessages(chat.id));
    // Simulate typing indicator
    const typingTimeout = setTimeout(() => {
      setIsTyping(true);
      setTimeout(() => setIsTyping(false), 3000);
    }, 2000);
    return () => clearTimeout(typingTimeout);
  }, [chat.id]);

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages, isTyping]);

  const handleSend = () => {
    if (!inputValue.trim()) return;

    const newMessage: Message = {
      id: `m-${Date.now()}`,
      senderId: currentUser.id,
      content: inputValue,
      timestamp: new Date(),
      status: 'sending',
      type: 'text',
    };

    setMessages((prev) => [...prev, newMessage]);
    setInputValue('');

    // Simulate message status updates
    setTimeout(() => {
      setMessages((prev) =>
        prev.map((m) =>
          m.id === newMessage.id ? { ...m, status: 'sent' } : m
        )
      );
    }, 500);

    setTimeout(() => {
      setMessages((prev) =>
        prev.map((m) =>
          m.id === newMessage.id ? { ...m, status: 'delivered' } : m
        )
      );
    }, 1000);

    setTimeout(() => {
      setMessages((prev) =>
        prev.map((m) =>
          m.id === newMessage.id ? { ...m, status: 'read' } : m
        )
      );
    }, 2000);
  };

  const getStatusText = () => {
    if (isGroup) {
      return `${chat.participants.length} members`;
    }
    if (participant.status === 'online') {
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
            {participant.status === 'online' && !isGroup && (
              <div className="absolute bottom-0 right-0 w-3 h-3 rounded-full bg-online border-2 border-background" />
            )}
          </div>

          <div className="min-w-0">
            <h2 className="font-semibold text-foreground truncate">{displayName}</h2>
            <p className={cn(
              'text-xs',
              participant.status === 'online' ? 'text-online' : 'text-muted-foreground'
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
      <div className="flex-1 overflow-y-auto px-4 py-4 scrollbar-hide">
        {/* Date separator */}
        <div className="flex justify-center mb-4">
          <div className="glass-card px-4 py-1.5 text-xs text-muted-foreground">
            Today
          </div>
        </div>

        {messages.map((message) => (
          <MessageBubble
            key={message.id}
            message={message}
            isOwn={message.senderId === currentUser.id}
          />
        ))}

        {isTyping && <TypingIndicator />}

        <div ref={messagesEndRef} />
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
              onChange={(e) => setInputValue(e.target.value)}
              onKeyDown={(e) => e.key === 'Enter' && handleSend()}
              className="w-full glass-input px-4 py-3 pr-24 text-sm text-foreground placeholder:text-muted-foreground"
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
