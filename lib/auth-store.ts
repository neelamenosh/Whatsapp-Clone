// Unified auth store - uses Supabase when configured, falls back to localStorage

import { isSupabaseConfigured } from './supabase/client';
import * as supabaseUsers from './supabase/users';
import type { User } from './types';

// Session storage key (per-tab user session)
const SESSION_USER_KEY = 'whatsapp_session_user';
// Local storage keys for fallback
const REGISTERED_USERS_KEY = 'whatsapp_enterprise_users';

// Default avatars
const DEFAULT_AVATARS = [
  'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1494790108377-be9c29b29330?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1438761681033-6461ffad8d80?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1534528741775-53994a69daeb?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?w=100&h=100&fit=crop&crop=face',
];

function getRandomAvatar(): string {
  return DEFAULT_AVATARS[Math.floor(Math.random() * DEFAULT_AVATARS.length)];
}

// Convert Supabase user to app User type
function supabaseToAppUser(su: supabaseUsers.User): User {
  return {
    id: su.id,
    name: su.displayName,
    email: su.email,
    avatar: su.avatar || getRandomAvatar(),
    status: su.status || 'online',
    about: su.bio || 'Available',
    phone: su.phone,
  };
}

// ==================== Supabase Methods ====================

async function registerWithSupabase(userData: {
  email: string;
  firstName: string;
  lastName: string;
  phone?: string;
  password: string;
}): Promise<{ user: User | null; error: string | null }> {
  const displayName = `${userData.firstName} ${userData.lastName}`;
  const username = userData.email.split('@')[0] + '_' + Date.now().toString(36);
  
  const result = await supabaseUsers.registerUser(
    userData.email,
    username,
    displayName,
    userData.password
  );

  if (result.error || !result.user) {
    return { user: null, error: result.error || 'Registration failed' };
  }

  const appUser = supabaseToAppUser(result.user);
  
  // Store in sessionStorage for this tab
  sessionStorage.setItem(SESSION_USER_KEY, JSON.stringify(appUser));
  
  return { user: appUser, error: null };
}

async function loginWithSupabase(
  email: string,
  password: string
): Promise<{ user: User | null; error: string | null }> {
  const result = await supabaseUsers.loginUser(email, password);

  if (result.error || !result.user) {
    return { user: null, error: result.error || 'Login failed' };
  }

  const appUser = supabaseToAppUser(result.user);
  
  // Store in sessionStorage for this tab only
  sessionStorage.setItem(SESSION_USER_KEY, JSON.stringify(appUser));
  
  return { user: appUser, error: null };
}

async function getAllUsersFromSupabase(): Promise<User[]> {
  const users = await supabaseUsers.getAllUsers();
  return users.map(supabaseToAppUser);
}

// ==================== LocalStorage Fallback Methods ====================

function getLocalUsers(): User[] {
  if (typeof window === 'undefined') return [];
  try {
    const stored = localStorage.getItem(REGISTERED_USERS_KEY);
    return stored ? JSON.parse(stored) : [];
  } catch {
    return [];
  }
}

function saveLocalUsers(users: User[]): void {
  if (typeof window === 'undefined') return;
  localStorage.setItem(REGISTERED_USERS_KEY, JSON.stringify(users));
}

function registerWithLocal(userData: {
  email: string;
  firstName: string;
  lastName: string;
  phone?: string;
}): { user: User | null; error: string | null } {
  const users = getLocalUsers();
  
  // Check if email exists
  if (users.some(u => u.email?.toLowerCase() === userData.email.toLowerCase())) {
    return { user: null, error: 'Email already registered' };
  }

  const newUser: User = {
    id: `user-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`,
    name: `${userData.firstName} ${userData.lastName}`,
    email: userData.email.toLowerCase(),
    avatar: getRandomAvatar(),
    status: 'online',
    about: 'Available',
    phone: userData.phone,
  };

  users.push(newUser);
  saveLocalUsers(users);
  
  // Store in sessionStorage for this tab
  sessionStorage.setItem(SESSION_USER_KEY, JSON.stringify(newUser));
  
  return { user: newUser, error: null };
}

function loginWithLocal(email: string): { user: User | null; error: string | null } {
  const users = getLocalUsers();
  const user = users.find(u => u.email?.toLowerCase() === email.toLowerCase());
  
  if (!user) {
    return { user: null, error: 'No account found with this email' };
  }

  const loggedInUser = { ...user, status: 'online' as const };
  
  // Update in storage
  const updatedUsers = users.map(u => u.id === user.id ? loggedInUser : u);
  saveLocalUsers(updatedUsers);
  
  // Store in sessionStorage for this tab
  sessionStorage.setItem(SESSION_USER_KEY, JSON.stringify(loggedInUser));
  
  return { user: loggedInUser, error: null };
}

// ==================== Public API ====================

// Register a new user
export async function registerUser(userData: {
  email: string;
  firstName: string;
  lastName: string;
  phone?: string;
  password: string;
}): Promise<{ user: User | null; error: string | null }> {
  if (isSupabaseConfigured()) {
    return registerWithSupabase(userData);
  }
  return registerWithLocal(userData);
}

// Login user
export async function loginUser(
  email: string,
  password: string
): Promise<{ user: User | null; error: string | null }> {
  if (isSupabaseConfigured()) {
    return loginWithSupabase(email, password);
  }
  return loginWithLocal(email);
}

// Get current logged in user (from sessionStorage - per-tab)
export function getCurrentUser(): User | null {
  if (typeof window === 'undefined') return null;
  
  try {
    const stored = sessionStorage.getItem(SESSION_USER_KEY);
    if (!stored) return null;
    return JSON.parse(stored);
  } catch {
    return null;
  }
}

// Set current user (update in sessionStorage)
export function setCurrentUser(user: User): void {
  if (typeof window === 'undefined') return;
  sessionStorage.setItem(SESSION_USER_KEY, JSON.stringify(user));
}

// Clear current user (logout)
export function clearCurrentUser(): void {
  if (typeof window === 'undefined') return;
  
  const user = getCurrentUser();
  if (user && isSupabaseConfigured()) {
    // Update online status in Supabase
    supabaseUsers.updateUserStatus(user.id, 'offline');
  }
  
  sessionStorage.removeItem(SESSION_USER_KEY);
}

// Get all other users (for chat list)
export async function getOtherUsers(): Promise<User[]> {
  const currentUser = getCurrentUser();
  
  if (isSupabaseConfigured()) {
    const allUsers = await getAllUsersFromSupabase();
    return currentUser ? allUsers.filter(u => u.id !== currentUser.id) : allUsers;
  }
  
  const allUsers = getLocalUsers();
  return currentUser ? allUsers.filter(u => u.id !== currentUser.id) : allUsers;
}

// Get all registered users
export async function getAllRegisteredUsers(): Promise<User[]> {
  if (isSupabaseConfigured()) {
    return getAllUsersFromSupabase();
  }
  return getLocalUsers();
}

// Find user by email
export async function findUserByEmail(email: string): Promise<User | null> {
  const users = await getAllRegisteredUsers();
  return users.find(u => u.email?.toLowerCase() === email.toLowerCase()) || null;
}

// Find user by ID
export async function findUserById(id: string): Promise<User | null> {
  if (isSupabaseConfigured()) {
    const su = await supabaseUsers.getUserById(id);
    return su ? supabaseToAppUser(su) : null;
  }
  
  const users = getLocalUsers();
  return users.find(u => u.id === id) || null;
}

// Update user profile
export async function updateUserProfile(
  userId: string,
  updates: Partial<Pick<User, 'name' | 'avatar' | 'about' | 'phone' | 'status'>>
): Promise<{ user: User | null; error: string | null }> {
  if (isSupabaseConfigured()) {
    const result = await supabaseUsers.updateUserProfile(userId, {
      displayName: updates.name,
      avatar: updates.avatar,
      bio: updates.about,
      phone: updates.phone,
      status: updates.status,
    });
    
    if (result.user) {
      const appUser = supabaseToAppUser(result.user);
      // Update session
      const currentUser = getCurrentUser();
      if (currentUser?.id === userId) {
        setCurrentUser(appUser);
      }
      return { user: appUser, error: null };
    }
    return { user: null, error: result.error };
  }
  
  // Local update
  const users = getLocalUsers();
  const userIndex = users.findIndex(u => u.id === userId);
  
  if (userIndex === -1) {
    return { user: null, error: 'User not found' };
  }
  
  const updatedUser = { ...users[userIndex], ...updates };
  users[userIndex] = updatedUser;
  saveLocalUsers(users);
  
  // Update session if current user
  const currentUser = getCurrentUser();
  if (currentUser?.id === userId) {
    setCurrentUser(updatedUser);
  }
  
  return { user: updatedUser, error: null };
}

// Check if Supabase is configured (for UI to show appropriate messages)
export function isDatabaseConfigured(): boolean {
  return isSupabaseConfigured();
}

// Update current user's online status
export async function updateOnlineStatus(status: 'online' | 'offline' | 'away'): Promise<void> {
  const user = getCurrentUser();
  if (!user) return;
  
  if (isSupabaseConfigured()) {
    await supabaseUsers.updateUserStatus(user.id, status);
  }
  
  // Also update session storage
  setCurrentUser({ ...user, status });
}

// Initialize presence tracking (call on app mount)
export function initPresenceTracking(): () => void {
  if (typeof window === 'undefined') return () => {};
  
  const user = getCurrentUser();
  if (!user) return () => {};
  
  // Set online on init
  updateOnlineStatus('online');
  
  // Handle visibility changes
  const handleVisibilityChange = () => {
    if (document.hidden) {
      updateOnlineStatus('away');
    } else {
      updateOnlineStatus('online');
    }
  };
  
  // Handle before unload (user leaving)
  const handleBeforeUnload = () => {
    updateOnlineStatus('offline');
  };
  
  // Handle online/offline events
  const handleOnline = () => {
    updateOnlineStatus('online');
  };
  
  const handleOffline = () => {
    updateOnlineStatus('offline');
  };
  
  document.addEventListener('visibilitychange', handleVisibilityChange);
  window.addEventListener('beforeunload', handleBeforeUnload);
  window.addEventListener('online', handleOnline);
  window.addEventListener('offline', handleOffline);
  
  // Heartbeat to keep status updated (every 30 seconds)
  const heartbeatInterval = setInterval(() => {
    if (!document.hidden) {
      updateOnlineStatus('online');
    }
  }, 30000);
  
  return () => {
    document.removeEventListener('visibilitychange', handleVisibilityChange);
    window.removeEventListener('beforeunload', handleBeforeUnload);
    window.removeEventListener('online', handleOnline);
    window.removeEventListener('offline', handleOffline);
    clearInterval(heartbeatInterval);
  };
}

// Delete user account
export async function deleteUserAccount(): Promise<{ success: boolean; error: string | null }> {
  const user = getCurrentUser();
  if (!user) {
    return { success: false, error: 'No user logged in' };
  }

  try {
    if (isSupabaseConfigured()) {
      // Import dynamically to avoid circular dependency
      const { deleteUserMessages } = await import('./supabase/messages');
      
      // First delete all user's messages
      const messagesResult = await deleteUserMessages(user.id);
      if (messagesResult.error) {
        console.error('Failed to delete user messages:', messagesResult.error);
      }

      // Then delete the user
      const userResult = await supabaseUsers.deleteUser(user.id);
      if (userResult.error) {
        return { success: false, error: userResult.error };
      }
    } else {
      // Local deletion
      const users = getLocalUsers();
      const filteredUsers = users.filter(u => u.id !== user.id);
      saveLocalUsers(filteredUsers);
    }

    // Clear session
    clearCurrentUser();
    
    // Clear local storage related to this user
    if (typeof window !== 'undefined') {
      // Clear any user-specific data
      const keysToRemove: string[] = [];
      for (let i = 0; i < localStorage.length; i++) {
        const key = localStorage.key(i);
        if (key && (key.includes(user.id) || key.includes('whatsapp_messages_'))) {
          keysToRemove.push(key);
        }
      }
      keysToRemove.forEach(key => localStorage.removeItem(key));
    }

    return { success: true, error: null };
  } catch (err) {
    console.error('Delete account error:', err);
    return { success: false, error: 'Failed to delete account' };
  }
}