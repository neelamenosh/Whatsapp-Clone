'use client';

import * as React from 'react';
import { X, Mail, Search, Loader2, UserPlus, AlertCircle, Users } from 'lucide-react';
import { cn } from '@/lib/utils';
import { findUserByEmail, getCurrentUser, getOtherUsers } from '@/lib/auth-store';
import type { User, Chat } from '@/lib/types';
import { getConsistentChatId } from '@/lib/live-chat';

interface NewChatModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onStartChat: (chat: Chat) => void;
  existingChats: Chat[];
}

export function NewChatModal({ open, onOpenChange, onStartChat, existingChats }: NewChatModalProps) {
  const [email, setEmail] = React.useState('');
  const [isSearching, setIsSearching] = React.useState(false);
  const [foundUser, setFoundUser] = React.useState<User | null>(null);
  const [error, setError] = React.useState('');
  const [hasSearched, setHasSearched] = React.useState(false);
  const [registeredUsers, setRegisteredUsers] = React.useState<User[]>([]);
  const [isLoadingUsers, setIsLoadingUsers] = React.useState(false);
  const [showUsersList, setShowUsersList] = React.useState(true);

  React.useEffect(() => {
    function onKeyDown(e: KeyboardEvent) {
      if (e.key === 'Escape') onOpenChange(false);
    }
    if (open) window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [open, onOpenChange]);

  // Load registered users when modal opens
  React.useEffect(() => {
    if (open) {
      setIsLoadingUsers(true);
      getOtherUsers().then((users) => {
        setRegisteredUsers(users);
        setIsLoadingUsers(false);
      });
    }
  }, [open]);

  // Reset state when modal opens/closes
  React.useEffect(() => {
    if (!open) {
      setEmail('');
      setFoundUser(null);
      setError('');
      setHasSearched(false);
      setShowUsersList(true);
    }
  }, [open]);

  const validateEmail = (email: string) => {
    return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
  };

  const handleSearch = async (e: React.FormEvent) => {
    e.preventDefault();
    
    setError('');
    setFoundUser(null);
    setHasSearched(false);

    if (!email.trim()) {
      setError('Please enter an email address');
      return;
    }

    if (!validateEmail(email)) {
      setError('Please enter a valid email address');
      return;
    }

    // Check if searching for self
    const currentUser = getCurrentUser();
    if (currentUser && currentUser.email?.toLowerCase() === email.toLowerCase()) {
      setError('You cannot start a chat with yourself');
      return;
    }

    setIsSearching(true);

    // Search for user by email in the registered users (async for Supabase)
    const user = await findUserByEmail(email);

    setIsSearching(false);
    setHasSearched(true);

    if (user) {
      setFoundUser(user);
    } else {
      setError('No account found with this email address. The user may not have registered yet.');
    }
  };

  const handleStartChat = () => {
    if (!foundUser) return;
    startChatWithUser(foundUser);
  };

  const startChatWithUser = (user: User) => {
    const currentUser = getCurrentUser();
    if (!currentUser) return;

    // Generate consistent chat ID
    const consistentChatId = getConsistentChatId(currentUser.id, user.id);

    // Check if chat already exists with this user
    const existingChat = existingChats.find(
      chat => chat.type === 'individual' && chat.participants.some(p => p.id === user.id)
    );

    if (existingChat) {
      // If chat already exists, just open it (update to use consistent ID)
      onStartChat({ ...existingChat, id: consistentChatId });
      onOpenChange(false);
      return;
    }

    // Create a new chat with consistent ID
    const newChat: Chat = {
      id: consistentChatId,
      type: 'individual',
      participants: [user],
      unreadCount: 0,
      isPinned: false,
      isMuted: false,
      updatedAt: new Date(),
    };

    onStartChat(newChat);
    onOpenChange(false);
  };

  if (!open) return null;

  return (
    <div className="fixed inset-0 z-[100]">
      {/* Backdrop */}
      <button
        type="button"
        className="absolute inset-0 bg-black/30 backdrop-blur-sm"
        aria-label="Close modal"
        onClick={() => onOpenChange(false)}
      />

      {/* Modal */}
      <div className="absolute inset-0 flex items-center justify-center p-4">
        <div 
          className="relative w-full max-w-md rounded-3xl overflow-hidden"
          style={{
            background: 'linear-gradient(145deg, rgba(255,255,255,0.95) 0%, rgba(240,253,244,0.9) 50%, rgba(255,255,255,0.95) 100%)',
            backdropFilter: 'blur(24px)',
            WebkitBackdropFilter: 'blur(24px)',
            border: '1px solid rgba(134,239,172,0.3)',
            boxShadow: `
              0 24px 80px rgba(22,163,74,0.15),
              0 8px 32px rgba(0,0,0,0.1),
              inset 0 1px 0 rgba(255,255,255,0.8)
            `,
          }}
        >
          {/* Header */}
          <div className="flex items-center justify-between p-6 pb-4 border-b border-border/30">
            <div className="flex items-center gap-3">
              <div className="w-10 h-10 rounded-xl bg-primary/10 flex items-center justify-center">
                <UserPlus className="w-5 h-5 text-primary" />
              </div>
              <div>
                <h2 className="text-lg font-semibold text-foreground">New Chat</h2>
                <p className="text-sm text-muted-foreground">Start a conversation</p>
              </div>
            </div>
            <button
              type="button"
              onClick={() => onOpenChange(false)}
              className="w-10 h-10 rounded-xl flex items-center justify-center text-muted-foreground hover:text-foreground hover:bg-muted/50 transition-colors"
              aria-label="Close"
            >
              <X className="w-5 h-5" />
            </button>
          </div>

          {/* Content */}
          <div className="p-6 space-y-6">
            {/* Search Form */}
            <form onSubmit={handleSearch} className="space-y-4">
              <div className="space-y-2">
                <label htmlFor="recipient-email" className="block text-sm font-medium text-foreground">
                  Recipient's Email
                </label>
                <div className="relative">
                  <Mail className="absolute left-4 top-1/2 -translate-y-1/2 w-5 h-5 text-muted-foreground" />
                  <input
                    id="recipient-email"
                    type="email"
                    value={email}
                    onChange={(e) => {
                      setEmail(e.target.value);
                      setError('');
                      setFoundUser(null);
                      setHasSearched(false);
                    }}
                    className={cn(
                      "w-full pl-12 pr-4 py-3 bg-white/60 border rounded-xl",
                      "focus:outline-none focus:ring-2 focus:ring-primary/50 transition-all",
                      "placeholder:text-muted-foreground",
                      error ? "border-destructive" : "border-border/50"
                    )}
                    placeholder="Enter email address..."
                    disabled={isSearching}
                  />
                </div>
                {error && (
                  <div className="flex items-start gap-2 text-sm text-destructive">
                    <AlertCircle className="w-4 h-4 mt-0.5 shrink-0" />
                    <span>{error}</span>
                  </div>
                )}
              </div>

              <button
                type="submit"
                disabled={isSearching || !email.trim()}
                className={cn(
                  "w-full flex items-center justify-center gap-2 px-4 py-3",
                  "bg-primary text-primary-foreground font-medium rounded-xl",
                  "hover:bg-primary/90 transition-all",
                  "disabled:opacity-50 disabled:cursor-not-allowed"
                )}
              >
                {isSearching ? (
                  <>
                    <Loader2 className="w-5 h-5 animate-spin" />
                    Searching...
                  </>
                ) : (
                  <>
                    <Search className="w-5 h-5" />
                    Find User
                  </>
                )}
              </button>
            </form>

            {/* Found User */}
            {foundUser && (
              <div className="space-y-4">
                <div className="h-px bg-border/50" />
                
                <div className="p-4 rounded-2xl bg-white/60 border border-border/30">
                  <div className="flex items-center gap-4">
                    <div className="relative">
                      <img
                        src={foundUser.avatar}
                        alt={foundUser.name}
                        className="w-14 h-14 rounded-full object-cover"
                      />
                      <span 
                        className={cn(
                          "absolute bottom-0 right-0 w-4 h-4 rounded-full border-2 border-white",
                          foundUser.status === 'online' ? "bg-green-500" :
                          foundUser.status === 'away' ? "bg-yellow-500" : "bg-gray-400"
                        )}
                      />
                    </div>
                    <div className="flex-1 min-w-0">
                      <h3 className="font-semibold text-foreground">{foundUser.name}</h3>
                      <p className="text-sm text-muted-foreground truncate">{foundUser.email}</p>
                      <p className="text-xs text-muted-foreground mt-0.5">{foundUser.about}</p>
                    </div>
                  </div>
                </div>

                <button
                  type="button"
                  onClick={handleStartChat}
                  className={cn(
                    "w-full flex items-center justify-center gap-2 px-4 py-3",
                    "bg-primary text-primary-foreground font-medium rounded-xl",
                    "hover:bg-primary/90 transition-all"
                  )}
                >
                  <UserPlus className="w-5 h-5" />
                  Start Conversation
                </button>
              </div>
            )}

            {/* No User Found Message */}
            {hasSearched && !foundUser && !error && (
              <div className="text-center py-4">
                <div className="w-16 h-16 mx-auto rounded-full bg-muted/50 flex items-center justify-center mb-3">
                  <AlertCircle className="w-8 h-8 text-muted-foreground" />
                </div>
                <p className="text-muted-foreground">No user found with this email</p>
              </div>
            )}

            {/* Registered Users List */}
            {!foundUser && !hasSearched && (
              <div className="space-y-4">
                <div className="h-px bg-border/50" />
                
                <div className="flex items-center gap-2">
                  <Users className="w-4 h-4 text-primary" />
                  <h3 className="text-sm font-medium text-foreground">Registered Users</h3>
                  <span className="text-xs text-muted-foreground">({registeredUsers.length})</span>
                </div>

                {isLoadingUsers ? (
                  <div className="flex items-center justify-center py-8">
                    <Loader2 className="w-6 h-6 animate-spin text-primary" />
                  </div>
                ) : registeredUsers.length === 0 ? (
                  <div className="text-center py-6">
                    <div className="w-12 h-12 mx-auto rounded-full bg-muted/50 flex items-center justify-center mb-2">
                      <Users className="w-6 h-6 text-muted-foreground" />
                    </div>
                    <p className="text-sm text-muted-foreground">No other users registered yet</p>
                  </div>
                ) : (
                  <div className="max-h-60 overflow-y-auto space-y-2 scrollbar-hide">
                    {registeredUsers.map((user) => (
                      <button
                        key={user.id}
                        type="button"
                        onClick={() => startChatWithUser(user)}
                        className={cn(
                          "w-full p-3 rounded-xl bg-white/60 border border-border/30",
                          "hover:bg-primary/5 hover:border-primary/30 transition-all",
                          "flex items-center gap-3 text-left"
                        )}
                      >
                        <div className="relative shrink-0">
                          <img
                            src={user.avatar}
                            alt={user.name}
                            className="w-11 h-11 rounded-full object-cover"
                          />
                          <span 
                            className={cn(
                              "absolute bottom-0 right-0 w-3 h-3 rounded-full border-2 border-white",
                              user.status === 'online' ? "bg-green-500" :
                              user.status === 'away' ? "bg-yellow-500" : "bg-gray-400"
                            )}
                          />
                        </div>
                        <div className="flex-1 min-w-0">
                          <h4 className="font-medium text-foreground text-sm">{user.name}</h4>
                          <p className="text-xs text-muted-foreground truncate">{user.email}</p>
                        </div>
                        <UserPlus className="w-4 h-4 text-primary shrink-0" />
                      </button>
                    ))}
                  </div>
                )}
              </div>
            )}
          </div>

          {/* Footer Hint */}
          <div className="px-6 pb-6">
            <p className="text-xs text-center text-muted-foreground">
              You can only message users who have an account in WhatsApp Enterprise
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
