import type { User, Chat, Message, Status, Call } from './types';

export const currentUser: User = {
  id: 'current-user',
  name: 'You',
  avatar: 'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face',
  status: 'online',
  about: 'Available',
};

export const users: User[] = [
  {
    id: '1',
    name: 'Sarah Chen',
    avatar: 'https://images.unsplash.com/photo-1494790108377-be9c29b29330?w=100&h=100&fit=crop&crop=face',
    status: 'online',
    about: 'Design Lead at Creative Co.',
  },
  {
    id: '2',
    name: 'Alex Rivera',
    avatar: 'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=100&h=100&fit=crop&crop=face',
    status: 'online',
    about: 'Building the future',
  },
  {
    id: '3',
    name: 'Jordan Kim',
    avatar: 'https://images.unsplash.com/photo-1438761681033-6461ffad8d80?w=100&h=100&fit=crop&crop=face',
    status: 'away',
    lastSeen: new Date(Date.now() - 1800000),
    about: 'Product Manager',
  },
  {
    id: '4',
    name: 'Design Team',
    avatar: 'https://images.unsplash.com/photo-1522071820081-009f0129c71c?w=100&h=100&fit=crop&crop=face',
    status: 'online',
    about: 'Official design team channel',
  },
  {
    id: '5',
    name: 'Emma Watson',
    avatar: 'https://images.unsplash.com/photo-1534528741775-53994a69daeb?w=100&h=100&fit=crop&crop=face',
    status: 'offline',
    lastSeen: new Date(Date.now() - 7200000),
    about: 'Coffee enthusiast',
  },
  {
    id: '6',
    name: 'Michael Scott',
    avatar: 'https://images.unsplash.com/photo-1500648767791-00dcc994a43e?w=100&h=100&fit=crop&crop=face',
    status: 'online',
    about: "World's best boss",
  },
  {
    id: '7',
    name: 'Lisa Park',
    avatar: 'https://images.unsplash.com/photo-1544005313-94ddf0286df2?w=100&h=100&fit=crop&crop=face',
    status: 'away',
    lastSeen: new Date(Date.now() - 900000),
    about: 'UX Researcher',
  },
];

const createMessages = (chatId: string): Message[] => {
  const messageTemplates: Record<string, Message[]> = {
    '1': [
      {
        id: 'm1',
        senderId: '1',
        content: 'Hey! How are you doing?',
        timestamp: new Date(Date.now() - 3600000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm2',
        senderId: 'current-user',
        content: "I'm great! Just finished the new designs.",
        timestamp: new Date(Date.now() - 3500000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm3',
        senderId: '1',
        content: 'Amazing! Can you share them with me?',
        timestamp: new Date(Date.now() - 3400000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm4',
        senderId: 'current-user',
        content: 'Sure! Let me send them over now.',
        timestamp: new Date(Date.now() - 3300000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm5',
        senderId: '1',
        content: 'Perfect! See you tomorrow then',
        timestamp: new Date(Date.now() - 240000),
        status: 'read',
        type: 'text',
      },
    ],
    '2': [
      {
        id: 'm1',
        senderId: '2',
        content: 'The new feature is live!',
        timestamp: new Date(Date.now() - 7200000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm2',
        senderId: 'current-user',
        content: 'Awesome work! The team will love this.',
        timestamp: new Date(Date.now() - 7100000),
        status: 'read',
        type: 'text',
      },
    ],
    '4': [
      {
        id: 'm1',
        senderId: '1',
        content: 'Team standup in 10 minutes',
        timestamp: new Date(Date.now() - 1800000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm2',
        senderId: '2',
        content: "I'll be there!",
        timestamp: new Date(Date.now() - 1700000),
        status: 'read',
        type: 'text',
      },
      {
        id: 'm3',
        senderId: '3',
        content: 'Shared the final mockups',
        timestamp: new Date(Date.now() - 900000),
        status: 'delivered',
        type: 'text',
      },
    ],
  };
  return messageTemplates[chatId] || [];
};

export const chats: Chat[] = [
  {
    id: '1',
    type: 'individual',
    participants: [users[0]],
    lastMessage: {
      id: 'm5',
      senderId: '1',
      content: 'Perfect! See you tomorrow then',
      timestamp: new Date(Date.now() - 240000),
      status: 'read',
      type: 'text',
    },
    unreadCount: 0,
    isPinned: true,
    isMuted: false,
    updatedAt: new Date(Date.now() - 240000),
  },
  {
    id: '4',
    type: 'group',
    participants: [users[0], users[1], users[2]],
    lastMessage: {
      id: 'm3',
      senderId: '2',
      content: 'Alex: Shared the final mockups',
      timestamp: new Date(Date.now() - 900000),
      status: 'delivered',
      type: 'text',
    },
    unreadCount: 3,
    isPinned: false,
    isMuted: false,
    updatedAt: new Date(Date.now() - 900000),
  },
  {
    id: '2',
    type: 'individual',
    participants: [users[1]],
    lastMessage: {
      id: 'm2',
      senderId: 'current-user',
      content: 'Awesome work! The team will love this.',
      timestamp: new Date(Date.now() - 7100000),
      status: 'read',
      type: 'text',
    },
    unreadCount: 0,
    isPinned: false,
    isMuted: false,
    updatedAt: new Date(Date.now() - 7100000),
  },
  {
    id: '3',
    type: 'individual',
    participants: [users[2]],
    lastMessage: {
      id: 'm1',
      senderId: '3',
      content: 'Can we reschedule our meeting?',
      timestamp: new Date(Date.now() - 14400000),
      status: 'read',
      type: 'text',
    },
    unreadCount: 0,
    isPinned: false,
    isMuted: true,
    updatedAt: new Date(Date.now() - 14400000),
  },
  {
    id: '5',
    type: 'individual',
    participants: [users[4]],
    lastMessage: {
      id: 'm1',
      senderId: '5',
      content: 'Thanks for the coffee recommendation!',
      timestamp: new Date(Date.now() - 86400000),
      status: 'read',
      type: 'text',
    },
    unreadCount: 0,
    isPinned: false,
    isMuted: false,
    updatedAt: new Date(Date.now() - 86400000),
  },
  {
    id: '6',
    type: 'individual',
    participants: [users[5]],
    lastMessage: {
      id: 'm1',
      senderId: 'current-user',
      content: "That's what she said!",
      timestamp: new Date(Date.now() - 172800000),
      status: 'read',
      type: 'text',
    },
    unreadCount: 0,
    isPinned: false,
    isMuted: false,
    updatedAt: new Date(Date.now() - 172800000),
  },
];

export const statuses: Status[] = [
  {
    id: 's1',
    userId: '1',
    user: users[0],
    content: 'Working on something exciting!',
    mediaUrl: 'https://images.unsplash.com/photo-1517694712202-14dd9538aa97?w=400&h=600&fit=crop',
    mediaType: 'image',
    createdAt: new Date(Date.now() - 3600000),
    expiresAt: new Date(Date.now() + 82800000),
    viewedBy: [],
  },
  {
    id: 's2',
    userId: '2',
    user: users[1],
    content: 'Beautiful sunset today',
    mediaUrl: 'https://images.unsplash.com/photo-1495616811223-4d98c6e9c869?w=400&h=600&fit=crop',
    mediaType: 'image',
    createdAt: new Date(Date.now() - 7200000),
    expiresAt: new Date(Date.now() + 79200000),
    viewedBy: ['current-user'],
  },
  {
    id: 's3',
    userId: '5',
    user: users[4],
    content: 'Coffee time!',
    mediaUrl: 'https://images.unsplash.com/photo-1509042239860-f550ce710b93?w=400&h=600&fit=crop',
    mediaType: 'image',
    createdAt: new Date(Date.now() - 14400000),
    expiresAt: new Date(Date.now() + 72000000),
    viewedBy: ['current-user'],
  },
  {
    id: 's4',
    userId: '6',
    user: users[5],
    content: 'New project launch!',
    backgroundColor: '#6366f1',
    createdAt: new Date(Date.now() - 21600000),
    expiresAt: new Date(Date.now() + 64800000),
    viewedBy: [],
  },
];

export const calls: Call[] = [
  {
    id: 'c1',
    participants: [users[0]],
    type: 'video',
    status: 'incoming',
    startedAt: new Date(Date.now() - 1800000),
    endedAt: new Date(Date.now() - 1200000),
    duration: 600,
  },
  {
    id: 'c2',
    participants: [users[1]],
    type: 'voice',
    status: 'outgoing',
    startedAt: new Date(Date.now() - 7200000),
    endedAt: new Date(Date.now() - 6600000),
    duration: 600,
  },
  {
    id: 'c3',
    participants: [users[2]],
    type: 'voice',
    status: 'missed',
    startedAt: new Date(Date.now() - 86400000),
  },
  {
    id: 'c4',
    participants: [users[0], users[1]],
    type: 'video',
    status: 'outgoing',
    startedAt: new Date(Date.now() - 172800000),
    endedAt: new Date(Date.now() - 169200000),
    duration: 3600,
  },
];

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
