import type { User, Chat, Message, Status, Call } from './types';

// Current user will be set from user-store after login
export let currentUser: User = {
  id: 'current-user',
  name: 'You',
  email: '',
  avatar: 'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face',
  status: 'online',
  about: 'Available',
};

// Function to update current user after login
export function setCurrentUserData(user: User) {
  currentUser = user;
}

// Empty users array - users are now stored in user-store.ts
export const users: User[] = [];

const createMessages = (chatId: string): Message[] => {
  const messageTemplates: Record<string, Message[]> = {};
  return messageTemplates[chatId] || [];
};

export const chats: Chat[] = [];

export const statuses: Status[] = [];

export const calls: Call[] = [];

export const getMessages = (chatId: string): Message[] => {
  return createMessages(chatId);
};

export const getChatById = (chatId: string): Chat | undefined => {
  return chats.find((chat) => chat.id === chatId);
};

export const getUserById = (userId: string): User | undefined => {
  if (userId === 'current-user') return currentUser;
  return users.find((user) => user.id === userId);
};
