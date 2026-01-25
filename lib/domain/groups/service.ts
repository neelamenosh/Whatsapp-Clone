// Group Chat Management Service

import type { 
  GroupChat, 
  GroupMember, 
  GroupSettings, 
  GroupRole,
  CreateGroupRequest,
  UpdateGroupRequest,
  GroupInviteLink,
  GroupJoinRequest 
} from '@/lib/contracts/groups';
import type { EventBus, DomainEvent } from '@/lib/platform/events/types';
import { createEvent } from '@/lib/platform/events/types';

// ============================================================================
// EVENTS
// ============================================================================

export const GroupEventTypes = {
  GROUP_CREATED: 'GROUP_CREATED',
  GROUP_UPDATED: 'GROUP_UPDATED',
  GROUP_DELETED: 'GROUP_DELETED',
  MEMBER_ADDED: 'MEMBER_ADDED',
  MEMBER_REMOVED: 'MEMBER_REMOVED',
  MEMBER_LEFT: 'MEMBER_LEFT',
  MEMBER_ROLE_CHANGED: 'MEMBER_ROLE_CHANGED',
  INVITE_LINK_CREATED: 'INVITE_LINK_CREATED',
  INVITE_LINK_REVOKED: 'INVITE_LINK_REVOKED',
} as const;

export interface GroupCreatedPayload {
  group: GroupChat;
}

export interface MemberAddedPayload {
  groupId: string;
  member: GroupMember;
  addedBy: string;
}

export interface MemberRemovedPayload {
  groupId: string;
  memberId: string;
  removedBy: string;
}

export function groupCreatedEvent(payload: GroupCreatedPayload): DomainEvent<GroupCreatedPayload> {
  return createEvent(GroupEventTypes.GROUP_CREATED, payload);
}

export function memberAddedEvent(payload: MemberAddedPayload): DomainEvent<MemberAddedPayload> {
  return createEvent(GroupEventTypes.MEMBER_ADDED, payload);
}

export function memberRemovedEvent(payload: MemberRemovedPayload): DomainEvent<MemberRemovedPayload> {
  return createEvent(GroupEventTypes.MEMBER_REMOVED, payload);
}

// ============================================================================
// STORE INTERFACE
// ============================================================================

export interface GroupStore {
  createGroup(group: GroupChat): Promise<GroupChat>;
  getGroup(groupId: string): Promise<GroupChat | null>;
  updateGroup(groupId: string, updates: Partial<GroupChat>): Promise<GroupChat>;
  deleteGroup(groupId: string): Promise<void>;
  
  addMember(groupId: string, member: GroupMember): Promise<GroupChat>;
  removeMember(groupId: string, memberId: string): Promise<GroupChat>;
  updateMemberRole(groupId: string, memberId: string, role: GroupRole): Promise<GroupChat>;
  
  getGroupsForUser(userId: string): Promise<GroupChat[]>;
  isMember(groupId: string, userId: string): Promise<boolean>;
  getMemberRole(groupId: string, userId: string): Promise<GroupRole | null>;
  
  // Invite links
  createInviteLink(link: GroupInviteLink): Promise<GroupInviteLink>;
  getInviteLink(code: string): Promise<GroupInviteLink | null>;
  revokeInviteLink(linkId: string): Promise<void>;
  
  // Join requests
  createJoinRequest(request: GroupJoinRequest): Promise<GroupJoinRequest>;
  getJoinRequests(groupId: string): Promise<GroupJoinRequest[]>;
  updateJoinRequest(requestId: string, status: 'approved' | 'rejected', reviewedBy: string): Promise<GroupJoinRequest>;
}

// ============================================================================
// SERVICE
// ============================================================================

export interface CreateGroupCommand {
  name: string;
  description?: string;
  creatorId: string;
  memberIds: string[];
  avatarUrl?: string;
  settings?: Partial<GroupSettings>;
}

const DEFAULT_GROUP_SETTINGS: GroupSettings = {
  messagingPermission: 'everyone',
  editInfoPermission: 'admins_only',
  addMembersPermission: 'everyone',
  approvalRequired: false,
  inviteLinkEnabled: true,
};

export async function createGroup(
  store: GroupStore,
  command: CreateGroupCommand,
  eventBus?: EventBus | null
): Promise<GroupChat> {
  const groupId = `group-${Date.now()}-${Math.random().toString(36).slice(2)}`;
  const timestamp = new Date().toISOString();
  
  // Create members list with creator as owner
  const members: GroupMember[] = [
    {
      userId: command.creatorId,
      role: 'owner',
      joinedAt: timestamp,
    },
    ...command.memberIds
      .filter(id => id !== command.creatorId)
      .map(userId => ({
        userId,
        role: 'member' as GroupRole,
        joinedAt: timestamp,
        addedBy: command.creatorId,
      })),
  ];
  
  const group: GroupChat = {
    id: groupId,
    name: command.name,
    description: command.description,
    avatarUrl: command.avatarUrl,
    members,
    settings: { ...DEFAULT_GROUP_SETTINGS, ...command.settings },
    createdAt: timestamp,
    createdBy: command.creatorId,
    updatedAt: timestamp,
  };
  
  const saved = await store.createGroup(group);
  
  if (eventBus) {
    await eventBus.publish(groupCreatedEvent({ group: saved }));
  }
  
  return saved;
}

export async function addMembers(
  store: GroupStore,
  groupId: string,
  memberIds: string[],
  addedBy: string,
  eventBus?: EventBus | null
): Promise<GroupChat> {
  const group = await store.getGroup(groupId);
  if (!group) {
    const error = new Error('Group not found');
    (error as any).statusCode = 404;
    throw error;
  }
  
  // Check permissions
  const adderRole = await store.getMemberRole(groupId, addedBy);
  if (!adderRole) {
    const error = new Error('Not a member of this group');
    (error as any).statusCode = 403;
    throw error;
  }
  
  if (group.settings.addMembersPermission === 'admins_only' && 
      adderRole !== 'owner' && adderRole !== 'admin') {
    const error = new Error('Only admins can add members');
    (error as any).statusCode = 403;
    throw error;
  }
  
  const timestamp = new Date().toISOString();
  let updatedGroup = group;
  
  for (const memberId of memberIds) {
    const isMember = await store.isMember(groupId, memberId);
    if (!isMember) {
      const member: GroupMember = {
        userId: memberId,
        role: 'member',
        joinedAt: timestamp,
        addedBy,
      };
      
      updatedGroup = await store.addMember(groupId, member);
      
      if (eventBus) {
        await eventBus.publish(memberAddedEvent({
          groupId,
          member,
          addedBy,
        }));
      }
    }
  }
  
  return updatedGroup;
}

export async function removeMember(
  store: GroupStore,
  groupId: string,
  memberId: string,
  removedBy: string,
  eventBus?: EventBus | null
): Promise<GroupChat> {
  const removerRole = await store.getMemberRole(groupId, removedBy);
  const memberRole = await store.getMemberRole(groupId, memberId);
  
  if (!removerRole) {
    const error = new Error('Not a member of this group');
    (error as any).statusCode = 403;
    throw error;
  }
  
  // Only owner/admin can remove others
  if (memberId !== removedBy && removerRole !== 'owner' && removerRole !== 'admin') {
    const error = new Error('Only admins can remove members');
    (error as any).statusCode = 403;
    throw error;
  }
  
  // Can't remove owner
  if (memberRole === 'owner' && memberId !== removedBy) {
    const error = new Error('Cannot remove the group owner');
    (error as any).statusCode = 403;
    throw error;
  }
  
  const updated = await store.removeMember(groupId, memberId);
  
  if (eventBus) {
    await eventBus.publish(memberRemovedEvent({
      groupId,
      memberId,
      removedBy,
    }));
  }
  
  return updated;
}

export async function updateMemberRole(
  store: GroupStore,
  groupId: string,
  memberId: string,
  newRole: GroupRole,
  updatedBy: string,
  eventBus?: EventBus | null
): Promise<GroupChat> {
  const updaterRole = await store.getMemberRole(groupId, updatedBy);
  
  if (updaterRole !== 'owner') {
    const error = new Error('Only the owner can change roles');
    (error as any).statusCode = 403;
    throw error;
  }
  
  if (newRole === 'owner') {
    // Transfer ownership
    await store.updateMemberRole(groupId, updatedBy, 'admin');
  }
  
  return store.updateMemberRole(groupId, memberId, newRole);
}

export async function getGroup(store: GroupStore, groupId: string): Promise<GroupChat | null> {
  return store.getGroup(groupId);
}

export async function getGroupsForUser(store: GroupStore, userId: string): Promise<GroupChat[]> {
  return store.getGroupsForUser(userId);
}

// ============================================================================
// IN-MEMORY STORE
// ============================================================================

export class MemoryGroupStore implements GroupStore {
  private groups = new Map<string, GroupChat>();
  private inviteLinks = new Map<string, GroupInviteLink>();
  private joinRequests = new Map<string, GroupJoinRequest>();
  
  async createGroup(group: GroupChat): Promise<GroupChat> {
    this.groups.set(group.id, group);
    return group;
  }
  
  async getGroup(groupId: string): Promise<GroupChat | null> {
    return this.groups.get(groupId) ?? null;
  }
  
  async updateGroup(groupId: string, updates: Partial<GroupChat>): Promise<GroupChat> {
    const group = this.groups.get(groupId);
    if (!group) throw new Error('Group not found');
    
    const updated = { ...group, ...updates, updatedAt: new Date().toISOString() };
    this.groups.set(groupId, updated);
    return updated;
  }
  
  async deleteGroup(groupId: string): Promise<void> {
    this.groups.delete(groupId);
  }
  
  async addMember(groupId: string, member: GroupMember): Promise<GroupChat> {
    const group = this.groups.get(groupId);
    if (!group) throw new Error('Group not found');
    
    group.members.push(member);
    group.updatedAt = new Date().toISOString();
    return group;
  }
  
  async removeMember(groupId: string, memberId: string): Promise<GroupChat> {
    const group = this.groups.get(groupId);
    if (!group) throw new Error('Group not found');
    
    group.members = group.members.filter(m => m.userId !== memberId);
    group.updatedAt = new Date().toISOString();
    return group;
  }
  
  async updateMemberRole(groupId: string, memberId: string, role: GroupRole): Promise<GroupChat> {
    const group = this.groups.get(groupId);
    if (!group) throw new Error('Group not found');
    
    const member = group.members.find(m => m.userId === memberId);
    if (member) {
      member.role = role;
    }
    group.updatedAt = new Date().toISOString();
    return group;
  }
  
  async getGroupsForUser(userId: string): Promise<GroupChat[]> {
    return Array.from(this.groups.values())
      .filter(g => g.members.some(m => m.userId === userId));
  }
  
  async isMember(groupId: string, userId: string): Promise<boolean> {
    const group = this.groups.get(groupId);
    return group?.members.some(m => m.userId === userId) ?? false;
  }
  
  async getMemberRole(groupId: string, userId: string): Promise<GroupRole | null> {
    const group = this.groups.get(groupId);
    const member = group?.members.find(m => m.userId === userId);
    return member?.role ?? null;
  }
  
  async createInviteLink(link: GroupInviteLink): Promise<GroupInviteLink> {
    this.inviteLinks.set(link.code, link);
    return link;
  }
  
  async getInviteLink(code: string): Promise<GroupInviteLink | null> {
    return this.inviteLinks.get(code) ?? null;
  }
  
  async revokeInviteLink(linkId: string): Promise<void> {
    for (const [code, link] of this.inviteLinks) {
      if (link.id === linkId) {
        link.isRevoked = true;
      }
    }
  }
  
  async createJoinRequest(request: GroupJoinRequest): Promise<GroupJoinRequest> {
    this.joinRequests.set(request.id, request);
    return request;
  }
  
  async getJoinRequests(groupId: string): Promise<GroupJoinRequest[]> {
    return Array.from(this.joinRequests.values())
      .filter(r => r.groupId === groupId && r.status === 'pending');
  }
  
  async updateJoinRequest(
    requestId: string, 
    status: 'approved' | 'rejected', 
    reviewedBy: string
  ): Promise<GroupJoinRequest> {
    const request = this.joinRequests.get(requestId);
    if (!request) throw new Error('Request not found');
    
    request.status = status;
    request.reviewedBy = reviewedBy;
    request.reviewedAt = new Date().toISOString();
    return request;
  }
}

let groupStore: GroupStore | null = null;

export function getGroupStore(): GroupStore {
  if (!groupStore) {
    groupStore = new MemoryGroupStore();
  }
  return groupStore;
}
