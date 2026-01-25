'use client';

import * as React from 'react';
import { cn } from '@/lib/utils';
import { getCurrentUser, setCurrentUser, getAllRegisteredUsers } from '@/lib/auth-store';
import type { User } from '@/lib/types';
import { 
  X, 
  Camera, 
  Pencil, 
  Check, 
  User as UserIcon,
  Mail,
  Phone,
  Info,
  Trash2,
  Image as ImageIcon
} from 'lucide-react';

type ProfileModalProps = {
  open: boolean;
  onOpenChange: (open: boolean) => void;
};

// Predefined avatar options
const AVATAR_OPTIONS = [
  'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1494790108377-be9c29b29330?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1438761681033-6461ffad8d80?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1534528741775-53994a69daeb?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1544005313-94ddf0286df2?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1506794778202-cad84cf45f1d?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1517841905240-472988babdf9?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1539571696357-5a69c17a67c6?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1524504388940-b1c1722653e1?w=150&h=150&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1488426862026-3ee34a7d66df?w=150&h=150&fit=crop&crop=face',
];

// Predefined about/status options like WhatsApp
const ABOUT_OPTIONS = [
  'Available',
  'Busy',
  'At school',
  'At the movies',
  'At work',
  'Battery about to die',
  'Can\'t talk, WhatsApp only',
  'In a meeting',
  'At the gym',
  'Sleeping',
  'Urgent calls only',
];

export function ProfileModal({ open, onOpenChange }: ProfileModalProps) {
  const [user, setUser] = React.useState<User | null>(null);
  const [editingName, setEditingName] = React.useState(false);
  const [editingAbout, setEditingAbout] = React.useState(false);
  const [nameValue, setNameValue] = React.useState('');
  const [aboutValue, setAboutValue] = React.useState('');
  const [showAvatarPicker, setShowAvatarPicker] = React.useState(false);
  const [showAboutOptions, setShowAboutOptions] = React.useState(false);
  const nameInputRef = React.useRef<HTMLInputElement>(null);
  const aboutInputRef = React.useRef<HTMLInputElement>(null);

  // Load current user on open
  React.useEffect(() => {
    if (open) {
      const currentUser = getCurrentUser();
      if (currentUser) {
        setUser(currentUser);
        setNameValue(currentUser.name);
        setAboutValue(currentUser.about || 'Available');
      }
    }
  }, [open]);

  // Focus input when editing
  React.useEffect(() => {
    if (editingName && nameInputRef.current) {
      nameInputRef.current.focus();
      nameInputRef.current.select();
    }
  }, [editingName]);

  React.useEffect(() => {
    if (editingAbout && aboutInputRef.current) {
      aboutInputRef.current.focus();
      aboutInputRef.current.select();
    }
  }, [editingAbout]);

  // Handle escape key
  React.useEffect(() => {
    function onKeyDown(e: KeyboardEvent) {
      if (e.key === 'Escape') {
        if (showAvatarPicker) {
          setShowAvatarPicker(false);
        } else if (showAboutOptions) {
          setShowAboutOptions(false);
        } else if (editingName) {
          setEditingName(false);
          setNameValue(user?.name || '');
        } else if (editingAbout) {
          setEditingAbout(false);
          setAboutValue(user?.about || 'Available');
        } else {
          onOpenChange(false);
        }
      }
    }
    if (open) window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [open, onOpenChange, showAvatarPicker, showAboutOptions, editingName, editingAbout, user]);

  const updateUserProfile = (updates: Partial<User>) => {
    if (!user) return;
    
    const updatedUser = { ...user, ...updates };
    setUser(updatedUser);
    setCurrentUser(updatedUser);
    
    // Also update in registered users list
    if (typeof window !== 'undefined') {
      const REGISTERED_USERS_KEY = 'whatsapp_enterprise_users';
      try {
        const stored = localStorage.getItem(REGISTERED_USERS_KEY);
        if (stored) {
          const users: User[] = JSON.parse(stored);
          const updatedUsers = users.map(u => 
            u.id === user.id ? updatedUser : u
          );
          localStorage.setItem(REGISTERED_USERS_KEY, JSON.stringify(updatedUsers));
        }
      } catch {
        // Ignore errors
      }
    }
  };

  const handleSaveName = () => {
    if (nameValue.trim() && nameValue !== user?.name) {
      updateUserProfile({ name: nameValue.trim() });
    }
    setEditingName(false);
  };

  const handleSaveAbout = () => {
    if (aboutValue !== user?.about) {
      updateUserProfile({ about: aboutValue });
    }
    setEditingAbout(false);
    setShowAboutOptions(false);
  };

  const handleSelectAbout = (about: string) => {
    setAboutValue(about);
    updateUserProfile({ about });
    setShowAboutOptions(false);
    setEditingAbout(false);
  };

  const handleSelectAvatar = (avatar: string) => {
    updateUserProfile({ avatar });
    setShowAvatarPicker(false);
  };

  if (!open || !user) return null;

  return (
    <div
      className="fixed inset-0 z-50 flex items-end sm:items-center justify-center"
      role="dialog"
      aria-modal="true"
      aria-labelledby="profile-title"
    >
      {/* Backdrop */}
      <div
        className="absolute inset-0 bg-background/80 backdrop-blur-md"
        onClick={() => onOpenChange(false)}
      />

      {/* Modal */}
      <div className={cn(
        'relative w-full sm:max-w-md max-h-[90vh] overflow-hidden',
        'bg-background/95 backdrop-blur-xl',
        'sm:rounded-3xl rounded-t-3xl',
        'border border-glass-border/50',
        'shadow-2xl',
        'animate-in slide-in-from-bottom-4 sm:slide-in-from-bottom-0 sm:zoom-in-95',
        'duration-300'
      )}>
        {/* Header */}
        <div className="sticky top-0 z-10 glass-panel px-6 py-4 flex items-center justify-between border-b border-glass-border/30">
          <h2 id="profile-title" className="text-xl font-bold text-foreground">
            Profile
          </h2>
          <button
            type="button"
            onClick={() => onOpenChange(false)}
            className="p-2 -mr-2 rounded-full hover:bg-muted/50 transition-colors"
            aria-label="Close profile"
          >
            <X className="h-5 w-5 text-muted-foreground" />
          </button>
        </div>

        {/* Content */}
        <div className="overflow-y-auto max-h-[calc(90vh-80px)] scrollbar-hide">
          {/* Avatar Section */}
          <div className="flex flex-col items-center py-8 px-6">
            <div className="relative group">
              <div className={cn(
                'w-40 h-40 rounded-full overflow-hidden',
                'ring-4 ring-primary/20',
                'shadow-2xl shadow-primary/20',
                'transition-transform duration-300 group-hover:scale-105'
              )}>
                <img
                  src={user.avatar || "/placeholder.svg"}
                  alt={user.name}
                  className="w-full h-full object-cover"
                  crossOrigin="anonymous"
                />
              </div>
              
              {/* Camera button overlay */}
              <button
                type="button"
                onClick={() => setShowAvatarPicker(true)}
                className={cn(
                  'absolute bottom-2 right-2',
                  'w-12 h-12 rounded-full',
                  'bg-primary text-primary-foreground',
                  'flex items-center justify-center',
                  'shadow-lg shadow-primary/40',
                  'transition-all duration-200',
                  'hover:scale-110 active:scale-95'
                )}
                aria-label="Change profile photo"
              >
                <Camera className="h-5 w-5" />
              </button>
            </div>

            {/* Online status indicator */}
            <div className="mt-4 flex items-center gap-2">
              <div className={cn(
                'w-3 h-3 rounded-full',
                user.status === 'online' ? 'bg-online online-pulse' : 
                user.status === 'away' ? 'bg-yellow-500' : 'bg-muted-foreground'
              )} />
              <span className="text-sm text-muted-foreground capitalize">
                {user.status}
              </span>
            </div>
          </div>

          {/* Profile Info Sections */}
          <div className="px-6 pb-8 space-y-1">
            {/* Name Section */}
            <div className="glass-card p-4">
              <div className="flex items-center gap-3 text-primary mb-2">
                <UserIcon className="h-5 w-5" />
                <span className="text-xs font-medium uppercase tracking-wider">Your Name</span>
              </div>
              
              {editingName ? (
                <div className="flex items-center gap-2">
                  <input
                    ref={nameInputRef}
                    type="text"
                    value={nameValue}
                    onChange={(e) => setNameValue(e.target.value)}
                    onKeyDown={(e) => e.key === 'Enter' && handleSaveName()}
                    maxLength={25}
                    className="flex-1 bg-transparent border-b-2 border-primary py-2 text-lg text-foreground focus:outline-none"
                  />
                  <button
                    type="button"
                    onClick={handleSaveName}
                    className="p-2 rounded-full bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                    aria-label="Save name"
                  >
                    <Check className="h-5 w-5" />
                  </button>
                </div>
              ) : (
                <div className="flex items-center justify-between">
                  <span className="text-lg text-foreground">{user.name}</span>
                  <button
                    type="button"
                    onClick={() => setEditingName(true)}
                    className="p-2 rounded-full hover:bg-muted/50 transition-colors text-primary"
                    aria-label="Edit name"
                  >
                    <Pencil className="h-5 w-5" />
                  </button>
                </div>
              )}
              
              <p className="text-xs text-muted-foreground mt-2">
                This is not your username or pin. This name will be visible to your contacts.
              </p>
            </div>

            {/* About Section */}
            <div className="glass-card p-4">
              <div className="flex items-center gap-3 text-primary mb-2">
                <Info className="h-5 w-5" />
                <span className="text-xs font-medium uppercase tracking-wider">About</span>
              </div>
              
              {editingAbout ? (
                <div className="space-y-3">
                  <div className="flex items-center gap-2">
                    <input
                      ref={aboutInputRef}
                      type="text"
                      value={aboutValue}
                      onChange={(e) => setAboutValue(e.target.value)}
                      onKeyDown={(e) => e.key === 'Enter' && handleSaveAbout()}
                      maxLength={139}
                      placeholder="Add about"
                      className="flex-1 bg-transparent border-b-2 border-primary py-2 text-lg text-foreground focus:outline-none"
                    />
                    <button
                      type="button"
                      onClick={handleSaveAbout}
                      className="p-2 rounded-full bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                      aria-label="Save about"
                    >
                      <Check className="h-5 w-5" />
                    </button>
                  </div>
                  
                  {/* Character count */}
                  <div className="text-right text-xs text-muted-foreground">
                    {aboutValue.length}/139
                  </div>

                  {/* Quick options */}
                  <button
                    type="button"
                    onClick={() => setShowAboutOptions(!showAboutOptions)}
                    className="text-sm text-primary hover:underline"
                  >
                    Select from options
                  </button>
                </div>
              ) : (
                <div className="flex items-center justify-between">
                  <span className="text-lg text-foreground">{user.about || 'Available'}</span>
                  <button
                    type="button"
                    onClick={() => setEditingAbout(true)}
                    className="p-2 rounded-full hover:bg-muted/50 transition-colors text-primary"
                    aria-label="Edit about"
                  >
                    <Pencil className="h-5 w-5" />
                  </button>
                </div>
              )}
            </div>

            {/* Phone Section */}
            <div className="glass-card p-4">
              <div className="flex items-center gap-3 text-primary mb-2">
                <Phone className="h-5 w-5" />
                <span className="text-xs font-medium uppercase tracking-wider">Phone</span>
              </div>
              <span className="text-lg text-foreground">
                {user.email?.includes('@') ? '+1 (555) 000-0000' : user.email || 'Not set'}
              </span>
              <p className="text-xs text-muted-foreground mt-2">
                Your phone number is used for account verification.
              </p>
            </div>

            {/* Email Section */}
            <div className="glass-card p-4">
              <div className="flex items-center gap-3 text-primary mb-2">
                <Mail className="h-5 w-5" />
                <span className="text-xs font-medium uppercase tracking-wider">Email</span>
              </div>
              <span className="text-lg text-foreground">{user.email || 'Not set'}</span>
            </div>
          </div>
        </div>

        {/* Avatar Picker Modal */}
        {showAvatarPicker && (
          <div className="absolute inset-0 z-20 bg-background/98 backdrop-blur-xl animate-in fade-in duration-200">
            <div className="sticky top-0 glass-panel px-6 py-4 flex items-center justify-between border-b border-glass-border/30">
              <h3 className="text-lg font-semibold text-foreground">Choose Photo</h3>
              <button
                type="button"
                onClick={() => setShowAvatarPicker(false)}
                className="p-2 -mr-2 rounded-full hover:bg-muted/50 transition-colors"
                aria-label="Close photo picker"
              >
                <X className="h-5 w-5 text-muted-foreground" />
              </button>
            </div>
            
            <div className="p-6 overflow-y-auto max-h-[calc(90vh-140px)]">
              {/* Current photo */}
              <div className="mb-6">
                <p className="text-sm text-muted-foreground mb-3">Current Photo</p>
                <div className="w-24 h-24 rounded-full overflow-hidden ring-2 ring-primary mx-auto">
                  <img
                    src={user.avatar || "/placeholder.svg"}
                    alt="Current"
                    className="w-full h-full object-cover"
                    crossOrigin="anonymous"
                  />
                </div>
              </div>

              {/* Avatar options */}
              <p className="text-sm text-muted-foreground mb-3">Choose an avatar</p>
              <div className="grid grid-cols-4 gap-3">
                {AVATAR_OPTIONS.map((avatar, index) => (
                  <button
                    key={index}
                    type="button"
                    onClick={() => handleSelectAvatar(avatar)}
                    className={cn(
                      'w-full aspect-square rounded-full overflow-hidden',
                      'ring-2 transition-all duration-200',
                      'hover:scale-105 hover:ring-primary',
                      user.avatar === avatar ? 'ring-primary ring-4' : 'ring-glass-border/50'
                    )}
                  >
                    <img
                      src={avatar}
                      alt={`Avatar ${index + 1}`}
                      className="w-full h-full object-cover"
                      crossOrigin="anonymous"
                    />
                  </button>
                ))}
              </div>

              {/* Remove photo option */}
              <button
                type="button"
                onClick={() => handleSelectAvatar('')}
                className="mt-6 w-full flex items-center justify-center gap-2 p-3 rounded-xl border border-destructive/30 text-destructive hover:bg-destructive/10 transition-colors"
              >
                <Trash2 className="h-5 w-5" />
                <span>Remove Photo</span>
              </button>
            </div>
          </div>
        )}

        {/* About Options Modal */}
        {showAboutOptions && (
          <div className="absolute inset-0 z-20 bg-background/98 backdrop-blur-xl animate-in fade-in duration-200">
            <div className="sticky top-0 glass-panel px-6 py-4 flex items-center justify-between border-b border-glass-border/30">
              <h3 className="text-lg font-semibold text-foreground">Select About</h3>
              <button
                type="button"
                onClick={() => setShowAboutOptions(false)}
                className="p-2 -mr-2 rounded-full hover:bg-muted/50 transition-colors"
                aria-label="Close about options"
              >
                <X className="h-5 w-5 text-muted-foreground" />
              </button>
            </div>
            
            <div className="p-4 overflow-y-auto max-h-[calc(90vh-140px)]">
              <div className="space-y-1">
                {ABOUT_OPTIONS.map((option) => (
                  <button
                    key={option}
                    type="button"
                    onClick={() => handleSelectAbout(option)}
                    className={cn(
                      'w-full p-4 rounded-xl text-left transition-colors',
                      'hover:bg-muted/50',
                      user.about === option ? 'bg-primary/10 text-primary' : 'text-foreground'
                    )}
                  >
                    <div className="flex items-center justify-between">
                      <span>{option}</span>
                      {user.about === option && (
                        <Check className="h-5 w-5 text-primary" />
                      )}
                    </div>
                  </button>
                ))}
              </div>
              
              {/* Custom option */}
              <button
                type="button"
                onClick={() => {
                  setShowAboutOptions(false);
                  setEditingAbout(true);
                }}
                className="mt-4 w-full p-4 rounded-xl text-left text-primary hover:bg-primary/10 transition-colors border border-primary/30"
              >
                <div className="flex items-center gap-2">
                  <Pencil className="h-5 w-5" />
                  <span>Write custom status</span>
                </div>
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
