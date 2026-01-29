'use client';

import { useState, useRef, useEffect, useCallback, useMemo } from 'react';
import { cn } from '@/lib/utils';
import type { Chat, Message } from '@/lib/types';
import { getCurrentUser } from '@/lib/auth-store';
import { Virtuoso } from 'react-virtuoso';
import { getLiveChatService, getConsistentChatId } from '@/lib/live-chat';
import { isSupabaseConfigured } from '@/lib/supabase/client';
import * as supabaseMessages from '@/lib/supabase/messages';
import * as supabaseUsers from '@/lib/supabase/users';
import { MessageBubble } from './message-bubble';
import {
  ChevronLeft,
  ExternalLink,
  Plus,
  Send,
  Smile,
  Mic,
  Search
} from 'lucide-react';

interface ConversationViewProps {
  chat: Chat;
  onBack: () => void;
  onMessageSent?: (chatId: string, message: Message) => void;
}

export function ConversationView({ chat, onBack, onMessageSent }: ConversationViewProps) {
  const [messages, setMessages] = useState<Message[]>([]);
  const [inputValue, setInputValue] = useState('');
  const [isTyping, setIsTyping] = useState(false);
  const [isOnline, setIsOnline] = useState(false);
  const inputRef = useRef<HTMLTextAreaElement>(null);
  const loggedInUser = getCurrentUser();
  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;

  const consistentChatId = useMemo(() => {
    return !isGroup && loggedInUser?.id
      ? getConsistentChatId(loggedInUser.id, participant.id)
      : chat.id;
  }, [isGroup, loggedInUser?.id, participant.id, chat.id]);

  // Load messages
  useEffect(() => {
    const loadMessages = async () => {
      if (isSupabaseConfigured() && loggedInUser?.id) {
        const supabaseMsgs = await supabaseMessages.getMessages(loggedInUser.id, participant.id);
        const formattedMsgs: Message[] = supabaseMsgs.map((msg) => ({
          id: msg.id,
          senderId: msg.senderId,
          content: msg.content,
          timestamp: new Date(msg.createdAt),
          status: msg.status,
          type: msg.type as any,
        }));
        setMessages(formattedMsgs);
      } else {
        const liveChatService = getLiveChatService();
        setMessages(liveChatService.getMessages(consistentChatId));
      }
    };
    loadMessages();
  }, [consistentChatId, participant.id, loggedInUser?.id]);

  // Handle Send
  const handleSend = () => {
    if (!inputValue.trim() || !loggedInUser) return;

    const newMessage: Message = {
      id: `m-${Date.now()}`,
      senderId: loggedInUser.id,
      content: inputValue.trim(),
      timestamp: new Date(),
      status: 'sent',
      type: 'text',
    };

    setMessages((prev) => [...prev, newMessage]);
    setInputValue('');
    if (inputRef.current) inputRef.current.style.height = 'auto';

    if (onMessageSent) onMessageSent(consistentChatId, newMessage);

    const liveChatService = getLiveChatService();
    liveChatService.sendMessage(consistentChatId, newMessage, participant.id);

    if (isSupabaseConfigured()) {
      supabaseMessages.sendMessage(loggedInUser.id, participant.id, newMessage.content, 'text');
    }
  };

  return (
    <div className="flex flex-col h-full bg-background animate-in slide-in-from-right duration-300 relative">
      {/* Header */}
      <div className="px-4 py-4 flex items-center justify-between border-b border-border sticky top-0 bg-background/80 backdrop-blur-md z-20">
        <button
          onClick={onBack}
          className="flex items-center gap-1 text-primary font-medium"
        >
          <ChevronLeft className="w-6 h-6" />
          <span className="text-base">Back</span>
        </button>
        <h2 className="text-base font-bold text-foreground">{displayName}</h2>
        <button className="text-primary p-2">
          <ExternalLink className="w-5 h-5" />
        </button>
      </div>

      {/* Message Area */}
      <div className="flex-1 flex flex-col relative overflow-hidden">
        {/* Tag Pill / Info Section */}
        <div className="flex flex-col items-center py-6 px-4 gap-4">
          <button className="px-5 py-2.5 bg-primary/10 text-primary rounded-full text-sm font-semibold hover:bg-primary/15 transition-colors border border-primary/20">
            Mainnet Design
          </button>

          <div className="flex flex-col items-center gap-2 text-center">
            <div className="flex items-center gap-2 text-muted-foreground/40 italic text-sm">
              <Search className="w-4 h-4" />
              <span>Searching conversations and files...</span>
            </div>
            <div className="h-px w-32 bg-border/50" />
          </div>
        </div>

        {/* Message List */}
        <div className="flex-1">
          <Virtuoso
            data={messages}
            initialTopMostItemIndex={messages.length - 1}
            followOutput="smooth"
            itemContent={(index, message) => (
              <div className="px-4 py-1">
                <MessageBubble
                  message={message}
                  isOwn={message.senderId === loggedInUser?.id}
                />
              </div>
            )}
            className="scrollbar-hide"
          />
        </div>
      </div>

      {/* Input Area */}
      <div className="p-4 bg-background border-t border-border safe-area-bottom">
        <div className="flex items-center gap-3">
          <button className="p-2 text-muted-foreground hover:text-primary transition-colors">
            <Plus className="w-6 h-6" />
          </button>

          <div className="flex-1 relative">
            <textarea
              ref={inputRef}
              rows={1}
              value={inputValue}
              onChange={(e) => {
                setInputValue(e.target.value);
                e.target.style.height = 'auto';
                e.target.style.height = `${Math.min(e.target.scrollHeight, 120)}px`;
              }}
              onKeyDown={(e) => {
                if (e.key === 'Enter' && !e.shiftKey) {
                  e.preventDefault();
                  handleSend();
                }
              }}
              placeholder="Message"
              className="w-full bg-muted/40 border-none rounded-2xl py-2.5 px-4 text-sm focus:ring-0 outline-none resize-none scrollbar-hide"
            />
            <button className="absolute right-2 top-1/2 -translate-y-1/2 p-2 text-muted-foreground hover:text-primary transition-colors">
              <Smile className="w-5 h-5" />
            </button>
          </div>

          <button
            onClick={handleSend}
            disabled={!inputValue.trim()}
            className={cn(
              "w-10 h-10 rounded-full flex items-center justify-center transition-all",
              inputValue.trim()
                ? "bg-primary text-white shadow-lg shadow-primary/30"
                : "bg-muted text-muted-foreground"
            )}
          >
            {inputValue.trim() ? (
              <Send className="w-5 h-5" />
            ) : (
              <Mic className="w-5 h-5" />
            )}
          </button>
        </div>
      </div>
    </div>
  );
}