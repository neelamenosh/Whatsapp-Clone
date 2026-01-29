export interface User {
  id: string;
  name: string;
  email?: string;
  avatar: string;
  status: 'online' | 'offline' | 'away';
  lastSeen?: Date;
  about?: string;
  phone?: string;
}

export interface Message {
  id: string;
  senderId: string;
  content: string;
  timestamp: Date;
  status: 'sending' | 'sent' | 'delivered' | 'read';
  type: 'text' | 'image' | 'voice' | 'location' | 'document' | 'video' | 'audio' | 'file';
  expiresAt?: Date;
  replyTo?: string;
  reactions?: { emoji: string; userId: string }[];
}

export interface Chat {
  id: string;
  type: 'individual' | 'group';
  participants: User[];
  lastMessage?: Message;
  unreadCount: number;
  isPinned: boolean;
  isMuted: boolean;
  updatedAt: Date;
}

export interface Status {
  id: string;
  userId: string;
  user: User;
  content: string;
  mediaUrl?: string;
  mediaType?: 'image' | 'video';
  backgroundColor?: string;
  createdAt: Date;
  expiresAt: Date;
  viewedBy: string[];
}

export interface Call {
  id: string;
  participants: User[];
  type: 'voice' | 'video';
  status: 'incoming' | 'outgoing' | 'missed' | 'declined';
  startedAt: Date;
  endedAt?: Date;
  duration?: number;
}

export type TabType = 'contacts' | 'calls' | 'chats' | 'node' | 'settings';

export interface Node {
  id: string;
  name: string;
  icon?: string;
  sections: NodeSection[];
}

export interface NodeSection {
  id: string;
  name: string;
  type: 'category' | 'external';
  icon?: string;
  channels: Channel[];
  isExpanded: boolean;
}

export interface Channel {
  id: string;
  name: string;
  type: 'text' | 'voice';
  unreadCount?: number;
  participants?: User[];
}
