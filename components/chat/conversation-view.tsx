'use client';

import { useState, useRef, useEffect, useCallback, useMemo } from 'react';
import { cn } from '@/lib/utils';
import type { Chat, Message } from '@/lib/types';
import { currentUser } from '@/lib/mock-data';
import { getCurrentUser } from '@/lib/auth-store';
import { formatLastSeen } from '@/lib/format';
import { useSettings } from '@/components/settings-provider';
import { Virtuoso } from 'react-virtuoso';
import { getLiveChatService, getConsistentChatId } from '@/lib/live-chat';
import { isSupabaseConfigured } from '@/lib/supabase/client';
import * as supabaseMessages from '@/lib/supabase/messages';
import * as supabaseUsers from '@/lib/supabase/users';
import { 
  encryptMessageForSending, 
  decryptReceivedMessage, 
  isE2EEEnabled,
  initializeE2EE,
} from '@/lib/encryption';
import { MessageBubble } from './message-bubble';
import { TypingIndicator } from './typing-indicator';
import { ContactProfileModal } from './contact-profile-modal';
import { 
  ArrowLeft, 
  Phone, 
  Video, 
  MoreVertical, 
  Smile, 
  Paperclip, 
  Mic, 
  Send,
  Camera,
  Search,
  X,
  User as UserIcon,
  Trash2,
  Ban,
  Unlock,
  ChevronUp,
  ChevronDown,
  Lock,
  ShieldCheck
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
  const [showMenu, setShowMenu] = useState(false);
  const [showContactProfile, setShowContactProfile] = useState(false);
  const [isE2EE, setIsE2EE] = useState(false);
  const [showSearch, setShowSearch] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [searchResults, setSearchResults] = useState<number[]>([]);
  const [currentSearchIndex, setCurrentSearchIndex] = useState(0);
  const [isBlockedBySupabase, setIsBlockedBySupabase] = useState(false);
  const [isClearing, setIsClearing] = useState(false);
  const [showClearConfirm, setShowClearConfirm] = useState(false);
  const [showBlockConfirm, setShowBlockConfirm] = useState(false);
  const inputRef = useRef<HTMLInputElement>(null);
  const searchInputRef = useRef<HTMLInputElement>(null);
  const typingTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const menuRef = useRef<HTMLDivElement>(null);
  const { settings, updateSettings } = useSettings();
  
  const loggedInUser = getCurrentUser();
  const participant = chat.participants[0];
  const isGroup = chat.type === 'group';
  const displayName = isGroup ? 'Design Team' : participant.name;
  
  // Memoize participant ID to prevent unnecessary re-renders
  const participantId = useMemo(() => participant.id, [participant.id]);
  const loggedInUserId = useMemo(() => loggedInUser?.id, [loggedInUser?.id]);
  
  // Check blocked status from both local settings and Supabase
  const isBlocked = !isGroup && (settings.privacy.blockedUserIds.includes(participantId) || isBlockedBySupabase);
  
  // Get consistent chat ID for message storage - memoized to prevent recalculation
  const consistentChatId = useMemo(() => {
    return !isGroup && loggedInUserId 
      ? getConsistentChatId(loggedInUserId, participantId) 
      : chat.id;
  }, [isGroup, loggedInUserId, participantId, chat.id]);

  // Close menu when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (menuRef.current && !menuRef.current.contains(event.target as Node)) {
        setShowMenu(false);
      }
    };

    if (showMenu) {
      document.addEventListener('mousedown', handleClickOutside);
    }

    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, [showMenu]);

  // Focus search input when search opens
  useEffect(() => {
    if (showSearch && searchInputRef.current) {
      searchInputRef.current.focus();
    }
  }, [showSearch]);

  // Search functionality
  useEffect(() => {
    if (searchQuery.trim() === '') {
      setSearchResults([]);
      setCurrentSearchIndex(0);
      return;
    }

    const query = searchQuery.toLowerCase();
    const results = messages
      .map((msg, index) => ({ index, matches: msg.content.toLowerCase().includes(query) }))
      .filter(item => item.matches)
      .map(item => item.index);
    
    setSearchResults(results);
    setCurrentSearchIndex(results.length > 0 ? 0 : -1);
  }, [searchQuery, messages]);

  // Check blocked status from Supabase on mount
  useEffect(() => {
    if (isSupabaseConfigured() && loggedInUserId && !isGroup) {
      supabaseUsers.isUserBlocked(loggedInUserId, participantId).then(blocked => {
        setIsBlockedBySupabase(blocked);
      });
    }
  }, [loggedInUserId, participantId, isGroup]);

  // Initialize E2EE on mount
  useEffect(() => {
    if (loggedInUserId) {
      // Check if E2EE is already enabled
      setIsE2EE(isE2EEEnabled());
      
      // Initialize E2EE (generates keys if not present, uploads public key)
      initializeE2EE(loggedInUserId).then(() => {
        setIsE2EE(isE2EEEnabled());
      }).catch(console.error);
    }
  }, [loggedInUserId]);

  // Load messages from Supabase or localStorage on mount
  useEffect(() => {
    let isMounted = true;
    
    const loadMessages = async () => {
      if (isSupabaseConfigured() && loggedInUserId) {
        // Load from Supabase
        const supabaseMsgs = await supabaseMessages.getMessages(loggedInUserId, participantId);
        if (!isMounted) return;
        
        // Decrypt messages if encrypted
        const formattedMsgs: Message[] = await Promise.all(supabaseMsgs.map(async (msg) => {
          let content = msg.content;
          
          // Decrypt if message is encrypted and from another user
          if (msg.encrypted && msg.senderId !== loggedInUserId) {
            content = await decryptReceivedMessage({
              content: msg.content,
              type: msg.type,
              encrypted: msg.encrypted,
              ciphertext: msg.ciphertext,
              nonce: msg.nonce,
              senderPublicKey: msg.senderPublicKey,
            }, msg.senderId);
          }
          
          return {
            id: msg.id,
            senderId: msg.senderId,
            content: content,
            timestamp: new Date(msg.createdAt),
            status: msg.status,
            type: msg.type,
          };
        }));
        
        setMessages(formattedMsgs);
        
        // Mark all received messages as read when opening the chat
        // This sends read receipts to the sender
        const chatId = supabaseMessages.getChatId(loggedInUserId, participantId);
        supabaseMessages.markMessagesAsRead(chatId, loggedInUserId);
      } else {
        // Fallback to localStorage
        const liveChatService = getLiveChatService();
        const storedMessages = liveChatService.getMessages(consistentChatId);
        if (!isMounted) return;
        setMessages(storedMessages);
      }
    };
    
    loadMessages();
    
    // Check if participant is online from Supabase
    const checkOnlineStatus = async () => {
      if (isSupabaseConfigured()) {
        const statusData = await supabaseUsers.getUserStatus(participantId);
        if (!isMounted) return;
        if (statusData) {
          setIsOnline(statusData.status === 'online');
        }
      } else {
        const liveChatService = getLiveChatService();
        if (!isMounted) return;
        setIsOnline(liveChatService.isUserOnline(participantId));
      }
    };
    checkOnlineStatus();
    
    return () => {
      isMounted = false;
    };
  }, [consistentChatId, participantId, loggedInUserId]);

  // Subscribe to participant's online status changes via Supabase
  useEffect(() => {
    if (!isSupabaseConfigured()) return;
    
    const unsubscribe = supabaseUsers.subscribeToUserStatus(participantId, (status, lastSeen) => {
      const wasOnline = isOnline;
      const nowOnline = status === 'online';
      setIsOnline(nowOnline);
      
      // If recipient just came online, update pending messages to 'delivered'
      if (!wasOnline && nowOnline && loggedInUserId) {
        setMessages((prev) =>
          prev.map((m) =>
            m.senderId === loggedInUserId && m.status === 'sent'
              ? { ...m, status: 'delivered' }
              : m
          )
        );
      }
    });

    return () => {
      if (unsubscribe) unsubscribe();
    };
  }, [participantId, isOnline, loggedInUserId]);

  // Subscribe to message status changes (for read receipts from recipient)
  useEffect(() => {
    if (!isSupabaseConfigured() || !loggedInUserId) return;
    
    const statusChannel = supabaseMessages.subscribeToMessageStatusChanges(
      loggedInUserId,
      (messageId, newStatus) => {
        // Update local message status when we get a status change notification
        setMessages((prev) =>
          prev.map((m) =>
            m.id === messageId ? { ...m, status: newStatus } : m
          )
        );
      }
    );

    return () => {
      if (statusChannel) {
        supabaseMessages.unsubscribe(statusChannel);
      }
    };
  }, [loggedInUserId]);

  // Listen for incoming messages (both Supabase real-time and localStorage)
  useEffect(() => {
    const liveChatService = getLiveChatService();
    
    // Track processed message IDs to prevent duplicates
    const processedIds = new Set<string>();
    
    // Subscribe to localStorage/BroadcastChannel updates
    const unsubMessage = liveChatService.onMessage((chatId, message) => {
      // Check if this message is for the current conversation
      if (chatId !== consistentChatId) return;
      
      // Skip if already processed
      if (processedIds.has(message.id)) return;
      processedIds.add(message.id);
      
      setMessages((prev) => {
        // Check for duplicates
        if (prev.some(m => m.id === message.id)) {
          return prev;
        }
        return [...prev, message];
      });
    });

    // Subscribe to Supabase real-time updates if configured
    let supabaseChannel: ReturnType<typeof supabaseMessages.subscribeToChat> = null;
    if (isSupabaseConfigured() && loggedInUserId) {
      const chatId = supabaseMessages.getChatId(loggedInUserId, participantId);
      supabaseChannel = supabaseMessages.subscribeToChat(chatId, (msg) => {
        // Only add if not from current user (to avoid duplicates)
        if (msg.senderId !== loggedInUserId) {
          // Skip if already processed
          if (processedIds.has(msg.id)) return;
          processedIds.add(msg.id);
          
          const formattedMsg: Message = {
            id: msg.id,
            senderId: msg.senderId,
            content: msg.content,
            timestamp: new Date(msg.createdAt),
            status: msg.status,
            type: msg.type,
          };
          setMessages((prev) => {
            if (prev.some(m => m.id === formattedMsg.id)) {
              return prev;
            }
            return [...prev, formattedMsg];
          });
          
          // Mark message as read immediately since chat is open
          // This sends read receipt back to the sender
          supabaseMessages.markMessagesAsRead(chatId, loggedInUserId);
        }
      });
    }

    return () => {
      unsubMessage();
      if (supabaseChannel) {
        supabaseMessages.unsubscribe(supabaseChannel);
      }
    };
  }, [consistentChatId, loggedInUserId, participantId]);

  // Listen for typing indicator
  useEffect(() => {
    const liveChatService = getLiveChatService();
    
    const unsubTyping = liveChatService.onTyping((chatId, userId, typing) => {
      if (chatId !== consistentChatId) return;
      if (userId === participantId) {
        setIsTyping(typing);
      }
    });

    return () => {
      unsubTyping();
    };
  }, [consistentChatId, participantId]);

  // Listen for online status changes
  useEffect(() => {
    const liveChatService = getLiveChatService();
    
    const unsubOnline = liveChatService.onOnlineStatus((userId, online) => {
      if (userId === participantId) {
        setIsOnline(online);
      }
    });

    // Periodically check online status - reduced frequency to prevent flickering
    const interval = setInterval(() => {
      setIsOnline(liveChatService.isUserOnline(participantId));
    }, 10000);

    return () => {
      unsubOnline();
      clearInterval(interval);
    };
  }, [participantId]);

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

    // Store the message content before clearing input
    const messageContent = inputValue.trim();
    
    // Generate client-side ID
    const messageId = `m-${Date.now()}-${Math.random().toString(16).slice(2)}`;

    const newMessage: Message = {
      id: messageId,
      senderId: loggedInUser.id,
      content: messageContent,
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
    
    // Notify parent to update chat's lastMessage in the chat list
    if (onMessageSent) {
      onMessageSent(consistentChatId, newMessage);
    }
    
    // Stop typing indicator
    const liveChatService = getLiveChatService();
    liveChatService.sendTyping(consistentChatId, false);
    
    // Send message to Supabase if configured (with E2EE)
    if (isSupabaseConfigured()) {
      // Encrypt the message for the recipient
      const encryptAndSend = async () => {
        let encryptedPayload: any = { encrypted: false };
        
        try {
          // Try to encrypt message using E2EE
          encryptedPayload = await encryptMessageForSending(
            messageContent,
            participant.id,
            'text'
          );
          
          console.log('🔐 Message encryption status:', {
            senderId: loggedInUser.id,
            recipientId: participant.id,
            encrypted: encryptedPayload.encrypted,
          });
        } catch (encryptErr) {
          console.warn('Encryption failed, sending unencrypted:', encryptErr);
          encryptedPayload = { encrypted: false };
        }
        
        try {
          const result = await supabaseMessages.sendMessage(
            loggedInUser.id,
            participant.id,
            messageContent,
            'text',
            encryptedPayload.encrypted ? {
              encrypted: true,
              ciphertext: encryptedPayload.ciphertext,
              nonce: encryptedPayload.nonce,
              senderPublicKey: encryptedPayload.senderPublicKey,
            } : undefined
          );
          
          if (result.error) {
            console.error('Failed to send message to Supabase:', result.error);
          } else if (result.message) {
            console.log('✅ Message sent successfully:', result.message.id);
            // Update message ID to match Supabase
            setMessages((prev) =>
              prev.map((m) =>
                m.id === messageId ? { ...m, id: result.message!.id } : m
              )
            );
          }
        } catch (err) {
          console.error('Send message error:', err);
        }
      };
      
      encryptAndSend();
    }
    
    // Also send via live chat service for real-time updates to other tabs
    liveChatService.sendMessage(consistentChatId, newMessage, participant.id);

    // If first message, notify recipient about new chat
    if (isFirstMessage && !isGroup) {
      liveChatService.notifyNewChat(
        { ...chat, id: consistentChatId, lastMessage: newMessage },
        participant.id
      );
    }

    // Update message status based on recipient's online status
    // - If recipient is OFFLINE: stays at 'sent' (single tick)
    // - If recipient is ONLINE: moves to 'delivered' (double tick)
    // - When recipient reads: moves to 'read' (blue/green double tick)
    const messageTimestamp = newMessage.timestamp.getTime();
    
    // After a short delay, mark as 'sent' (message reached server)
    setTimeout(() => {
      setMessages((prev) =>
        prev.map((m) =>
          (m.id === messageId || (m.senderId === loggedInUser.id && m.timestamp.getTime() === messageTimestamp)) 
            ? { ...m, status: 'sent' } 
            : m
        )
      );
      
      // If recipient is online, mark as delivered after another short delay
      if (isOnline) {
        setTimeout(() => {
          setMessages((prev) =>
            prev.map((m) =>
              (m.id === messageId || (m.senderId === loggedInUser.id && m.timestamp.getTime() === messageTimestamp)) 
                ? { ...m, status: 'delivered' } 
                : m
            )
          );
          
          // Update in Supabase if configured
          if (isSupabaseConfigured()) {
            supabaseMessages.updateMessageStatus(messageId, 'delivered').catch(console.error);
          }
        }, 300);
      }
    }, 300);
  };

  // Handle clear chat
  const handleClearChat = async () => {
    if (!loggedInUser) return;
    
    setIsClearing(true);
    
    try {
      // Clear from Supabase if configured
      if (isSupabaseConfigured()) {
        const result = await supabaseMessages.clearChat(loggedInUser.id, participant.id);
        if (result.error) {
          console.error('Failed to clear chat from Supabase:', result.error);
          alert('Failed to clear chat. Please try again.');
          setShowClearConfirm(false);
          return;
        }
      }
      
      // Clear local messages
      setMessages([]);
      
      // Clear from localStorage
      const liveChatService = getLiveChatService();
      localStorage.removeItem(`whatsapp_messages_${consistentChatId}`);
      
      setShowClearConfirm(false);
    } catch (err) {
      console.error('Error clearing chat:', err);
      alert('Failed to clear chat. Please try again.');
    } finally {
      setIsClearing(false);
    }
  };

  // Handle block user
  const handleBlockUser = async () => {
    if (!loggedInUser || isGroup) return;
    
    // Update local settings
    updateSettings((prev) => ({
      ...prev,
      privacy: {
        ...prev.privacy,
        blockedUserIds: [...prev.privacy.blockedUserIds, participant.id],
      },
    }));
    
    // Update in Supabase
    if (isSupabaseConfigured()) {
      const result = await supabaseUsers.blockUser(loggedInUser.id, participant.id);
      if (result.error) {
        console.error('Failed to block user in Supabase:', result.error);
      } else {
        setIsBlockedBySupabase(true);
      }
    }
    
    setShowBlockConfirm(false);
  };

  // Handle unblock user
  const handleUnblockUser = async () => {
    if (!loggedInUser || isGroup) return;
    
    // Update local settings
    updateSettings((prev) => ({
      ...prev,
      privacy: {
        ...prev.privacy,
        blockedUserIds: prev.privacy.blockedUserIds.filter(id => id !== participant.id),
      },
    }));
    
    // Update in Supabase
    if (isSupabaseConfigured()) {
      const result = await supabaseUsers.unblockUser(loggedInUser.id, participant.id);
      if (result.error) {
        console.error('Failed to unblock user in Supabase:', result.error);
      } else {
        setIsBlockedBySupabase(false);
      }
    }
  };

  // Navigate search results
  const goToNextResult = () => {
    if (searchResults.length === 0) return;
    setCurrentSearchIndex((prev) => (prev + 1) % searchResults.length);
  };

  const goToPrevResult = () => {
    if (searchResults.length === 0) return;
    setCurrentSearchIndex((prev) => (prev - 1 + searchResults.length) % searchResults.length);
  };

  // Get filtered messages for display
  const displayMessages = messages;

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
      {/* Contact Profile Modal */}
      <ContactProfileModal
        open={showContactProfile}
        onOpenChange={setShowContactProfile}
        contact={participant}
        isOnline={isOnline}
        isBlocked={isBlocked}
        onBlock={handleBlockUser}
        onUnblock={handleUnblockUser}
      />

      {/* Header */}
      <div className="glass-panel px-4 py-3 flex items-center gap-3 z-20">
        <button
          type="button"
          onClick={onBack}
          className="p-2 -ml-2 rounded-full hover:bg-muted/50 transition-colors"
          aria-label="Go back"
        >
          <ArrowLeft className="h-5 w-5 text-foreground" />
        </button>

        <div 
          className="flex items-center gap-3 flex-1 min-w-0 cursor-pointer hover:opacity-80 transition-opacity"
          onClick={() => !isGroup && setShowContactProfile(true)}
        >
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
            <div className="flex items-center gap-1.5">
              <h2 className="font-semibold text-foreground truncate">{displayName}</h2>
              {isE2EE && (
                <div className="flex items-center" title="End-to-end encrypted">
                  <Lock className="h-3.5 w-3.5 text-primary" />
                </div>
              )}
            </div>
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
          
          {/* More options dropdown */}
          <div className="relative" ref={menuRef}>
            <button
              type="button"
              onClick={() => setShowMenu(!showMenu)}
              className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
              aria-label="More options"
            >
              <MoreVertical className="h-5 w-5" />
            </button>
            
            {showMenu && (
              <div className="absolute right-0 top-12 w-56 glass-panel rounded-xl shadow-xl py-2 z-50 animate-in fade-in slide-in-from-top-2 duration-150">
                {/* View Contact */}
                {!isGroup && (
                  <button
                    type="button"
                    onClick={() => {
                      setShowMenu(false);
                      setShowContactProfile(true);
                    }}
                    className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
                  >
                    <UserIcon className="h-5 w-5 text-muted-foreground" />
                    <span className="text-sm text-foreground">View Contact</span>
                  </button>
                )}
                
                {/* Search */}
                <button
                  type="button"
                  onClick={() => {
                    setShowMenu(false);
                    setShowSearch(true);
                  }}
                  className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left"
                >
                  <Search className="h-5 w-5 text-muted-foreground" />
                  <span className="text-sm text-foreground">Search</span>
                </button>
                
                {/* Clear Chat */}
                <button
                  type="button"
                  onClick={() => {
                    setShowMenu(false);
                    setShowClearConfirm(true);
                  }}
                  disabled={isClearing}
                  className="w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left disabled:opacity-50"
                >
                  <Trash2 className="h-5 w-5 text-muted-foreground" />
                  <span className="text-sm text-foreground">
                    {isClearing ? 'Clearing...' : 'Clear Chat'}
                  </span>
                </button>
                
                <div className="h-px bg-border my-2" />
                
                {/* Block/Unblock */}
                {!isGroup && (
                  <button
                    type="button"
                    onClick={() => {
                      setShowMenu(false);
                      if (isBlocked) {
                        handleUnblockUser();
                      } else {
                        setShowBlockConfirm(true);
                      }
                    }}
                    className={cn(
                      'w-full px-4 py-2.5 flex items-center gap-3 hover:bg-muted/50 transition-colors text-left',
                      isBlocked ? 'text-primary' : 'text-destructive'
                    )}
                  >
                    {isBlocked ? (
                      <>
                        <Unlock className="h-5 w-5" />
                        <span className="text-sm">Unblock Contact</span>
                      </>
                    ) : (
                      <>
                        <Ban className="h-5 w-5" />
                        <span className="text-sm">Block Contact</span>
                      </>
                    )}
                  </button>
                )}
              </div>
            )}
          </div>
        </div>
      </div>

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
                <Trash2 className="h-8 w-8 text-destructive" />
              </div>
              <h3 className="text-lg font-semibold text-foreground mb-2">Clear Chat?</h3>
              <p className="text-sm text-muted-foreground mb-6">
                Are you sure you want to delete all messages with <span className="font-medium text-foreground">{participant.name}</span>? This action cannot be undone.
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

      {/* Block Contact Confirmation Modal */}
      {showBlockConfirm && (
        <div
          className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 backdrop-blur-sm"
          onClick={() => setShowBlockConfirm(false)}
        >
          <div
            className="w-full max-w-sm mx-4 glass-panel rounded-2xl shadow-2xl overflow-hidden animate-in fade-in zoom-in-95 duration-200"
            onClick={(e) => e.stopPropagation()}
          >
            <div className="p-6 text-center">
              <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-destructive/10 flex items-center justify-center">
                <Ban className="h-8 w-8 text-destructive" />
              </div>
              <h3 className="text-lg font-semibold text-foreground mb-2">Block {participant.name}?</h3>
              <p className="text-sm text-muted-foreground mb-6">
                Blocked contacts will no longer be able to send you messages. You can unblock them anytime.
              </p>
              <div className="flex gap-3">
                <button
                  type="button"
                  onClick={() => setShowBlockConfirm(false)}
                  className="flex-1 px-4 py-2.5 bg-muted hover:bg-muted/80 text-foreground font-medium rounded-xl transition-colors"
                >
                  Cancel
                </button>
                <button
                  type="button"
                  onClick={handleBlockUser}
                  className="flex-1 px-4 py-2.5 bg-destructive hover:bg-destructive/90 text-destructive-foreground font-medium rounded-xl transition-colors"
                >
                  Block
                </button>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Search Bar */}
      {showSearch && (
        <div className="glass-panel px-4 py-2 flex items-center gap-3 z-10 border-b border-border">
          <Search className="h-5 w-5 text-muted-foreground shrink-0" />
          <input
            ref={searchInputRef}
            type="text"
            placeholder="Search in conversation..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="flex-1 bg-transparent text-sm text-foreground placeholder:text-muted-foreground focus:outline-none"
          />
          {searchResults.length > 0 && (
            <div className="flex items-center gap-2">
              <span className="text-xs text-muted-foreground">
                {currentSearchIndex + 1} of {searchResults.length}
              </span>
              <button
                type="button"
                onClick={goToPrevResult}
                className="p-1 hover:bg-muted/50 rounded transition-colors"
                aria-label="Previous result"
              >
                <ChevronUp className="h-4 w-4 text-muted-foreground" />
              </button>
              <button
                type="button"
                onClick={goToNextResult}
                className="p-1 hover:bg-muted/50 rounded transition-colors"
                aria-label="Next result"
              >
                <ChevronDown className="h-4 w-4 text-muted-foreground" />
              </button>
            </div>
          )}
          <button
            type="button"
            onClick={() => {
              setShowSearch(false);
              setSearchQuery('');
            }}
            className="p-1 hover:bg-muted/50 rounded transition-colors"
            aria-label="Close search"
          >
            <X className="h-5 w-5 text-muted-foreground" />
          </button>
        </div>
      )}

      {/* Messages */}
      <div className="flex-1 overflow-hidden">
        <Virtuoso
          className="h-full px-4 py-4 scrollbar-hide"
          data={displayMessages}
          followOutput="smooth"
          itemContent={(index, message) => {
            const isSearchMatch = searchResults.includes(index);
            const isCurrentSearchMatch = searchResults[currentSearchIndex] === index;
            
            return (
              <div
                className={cn(
                  'transition-all duration-200',
                  isSearchMatch && 'bg-primary/10 -mx-2 px-2 rounded-lg',
                  isCurrentSearchMatch && 'bg-primary/20 ring-2 ring-primary/50'
                )}
              >
                <MessageBubble
                  key={message.id}
                  message={message}
                  isOwn={message.senderId === loggedInUser?.id}
                  searchQuery={searchQuery}
                />
              </div>
            );
          }}
          components={{
            Header: () => (
              <div className="space-y-4">
                {/* E2EE Banner */}
                {isE2EE && (
                  <div className="flex justify-center">
                    <div className="flex items-center gap-2 glass-card px-4 py-2 text-xs text-muted-foreground bg-primary/5 border-primary/20">
                      <ShieldCheck className="h-4 w-4 text-primary" />
                      <span>
                        Messages are end-to-end encrypted
                      </span>
                    </div>
                  </div>
                )}
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

      {/* Block/Unblock Bar - shown when user is blocked */}
      {isBlocked && !isGroup && (
        <div className="glass-panel px-4 py-3 border-t border-border">
          <div className="flex items-center justify-center gap-4">
            <p className="text-sm text-muted-foreground">
              You have blocked this contact
            </p>
            <button
              type="button"
              onClick={handleUnblockUser}
              className="px-4 py-2 bg-primary/10 hover:bg-primary/20 text-primary text-sm font-medium rounded-lg transition-colors"
            >
              Unblock
            </button>
          </div>
        </div>
      )}

      {/* Input - hide when blocked */}
      {!isBlocked && (
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
      )}
    </div>
  );
}