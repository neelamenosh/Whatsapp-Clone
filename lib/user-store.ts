import type { User } from './types';

// Key for localStorage
const REGISTERED_USERS_KEY = 'whatsapp_enterprise_users';
const CURRENT_USER_KEY = 'whatsapp_enterprise_current_user';

// Default avatar for new users
const DEFAULT_AVATARS = [
  'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1494790108377-be9c29b29330?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1438761681033-6461ffad8d80?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1534528741775-53994a69daeb?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?w=100&h=100&fit=crop&crop=face',
  'https://images.unsplash.com/photo-1544005313-94ddf0286df2?w=100&h=100&fit=crop&crop=face',
];

function getRandomAvatar(): string {
  return DEFAULT_AVATARS[Math.floor(Math.random() * DEFAULT_AVATARS.length)];
}

// Get all registered users from localStorage
export function getRegisteredUsers(): User[] {
  if (typeof window === 'undefined') return [];
  
  try {
    const stored = localStorage.getItem(REGISTERED_USERS_KEY);
    if (!stored) return [];
    return JSON.parse(stored);
  } catch {
    return [];
  }
}

// Save users to localStorage
function saveRegisteredUsers(users: User[]): void {
  if (typeof window === 'undefined') return;
  localStorage.setItem(REGISTERED_USERS_KEY, JSON.stringify(users));
}

// Register a new user
export function registerUser(userData: {
  email: string;
  firstName: string;
  lastName: string;
  phone?: string;
  jobTitle?: string;
  companyName?: string;
}): User {
  const users = getRegisteredUsers();
  
  // Check if user already exists
  const existingUser = users.find(u => u.email?.toLowerCase() === userData.email.toLowerCase());
  if (existingUser) {
    throw new Error('User with this email already exists');
  }
  
  const newUser: User = {
    id: `user-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`,
    name: `${userData.firstName} ${userData.lastName}`,
    email: userData.email.toLowerCase(),
    avatar: getRandomAvatar(),
    status: 'online',
    about: userData.jobTitle || 'Available',
  };
  
  users.push(newUser);
  saveRegisteredUsers(users);
  
  return newUser;
}

// Find user by email
export function findUserByEmail(email: string): User | undefined {
  const users = getRegisteredUsers();
  return users.find(u => u.email?.toLowerCase() === email.toLowerCase());
}

// Get current logged in user
export function getCurrentUser(): User | null {
  if (typeof window === 'undefined') return null;
  
  try {
    const stored = localStorage.getItem(CURRENT_USER_KEY);
    if (!stored) return null;
    return JSON.parse(stored);
  } catch {
    return null;
  }
}

// Set current logged in user
export function setCurrentUser(user: User): void {
  if (typeof window === 'undefined') return;
  localStorage.setItem(CURRENT_USER_KEY, JSON.stringify(user));
}

// Clear current user (logout)
export function clearCurrentUser(): void {
  if (typeof window === 'undefined') return;
  localStorage.removeItem(CURRENT_USER_KEY);
}

// Login user by email (returns user if found)
export function loginUser(email: string): User | null {
  const user = findUserByEmail(email);
  if (user) {
    // Update status to online
    const users = getRegisteredUsers();
    const updatedUsers = users.map(u => 
      u.id === user.id ? { ...u, status: 'online' as const } : u
    );
    saveRegisteredUsers(updatedUsers);
    
    const loggedInUser = { ...user, status: 'online' as const };
    setCurrentUser(loggedInUser);
    return loggedInUser;
  }
  return null;
}

// Get all users except current user (for searching contacts)
export function getOtherUsers(): User[] {
  const currentUser = getCurrentUser();
  const users = getRegisteredUsers();
  
  if (!currentUser) return users;
  return users.filter(u => u.id !== currentUser.id);
}
