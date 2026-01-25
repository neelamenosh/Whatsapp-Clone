import { z } from 'zod';

// ============================================================================
// GROUP CHAT MANAGEMENT
// ============================================================================

export const GroupRoleSchema = z.enum(['owner', 'admin', 'member']);
export type GroupRole = z.infer<typeof GroupRoleSchema>;

export const GroupMemberSchema = z.object({
  userId: z.string().min(1),
  role: GroupRoleSchema,
  joinedAt: z.string().datetime(),
  addedBy: z.string().min(1).optional(),
  nickname: z.string().max(50).optional(),
});

export type GroupMember = z.infer<typeof GroupMemberSchema>;

export const GroupSettingsSchema = z.object({
  // Who can send messages
  messagingPermission: z.enum(['everyone', 'admins_only']),
  // Who can edit group info
  editInfoPermission: z.enum(['everyone', 'admins_only']),
  // Who can add members
  addMembersPermission: z.enum(['everyone', 'admins_only']),
  // Require admin approval for new members
  approvalRequired: z.boolean(),
  // Allow members to invite via link
  inviteLinkEnabled: z.boolean(),
  // Disappearing messages setting
  disappearingMode: z.enum(['off', '24h', '7d', '90d']).optional(),
});

export type GroupSettings = z.infer<typeof GroupSettingsSchema>;

export const GroupChatSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1).max(100),
  description: z.string().max(500).optional(),
  avatarUrl: z.string().url().optional(),
  members: z.array(GroupMemberSchema),
  settings: GroupSettingsSchema,
  inviteLink: z.string().optional(),
  createdAt: z.string().datetime(),
  createdBy: z.string().min(1),
  updatedAt: z.string().datetime(),
});

export type GroupChat = z.infer<typeof GroupChatSchema>;

export const CreateGroupRequestSchema = z.object({
  name: z.string().min(1).max(100),
  description: z.string().max(500).optional(),
  memberIds: z.array(z.string().min(1)).min(1),
  avatarUrl: z.string().url().optional(),
  settings: GroupSettingsSchema.optional(),
});

export type CreateGroupRequest = z.infer<typeof CreateGroupRequestSchema>;

export const UpdateGroupRequestSchema = z.object({
  name: z.string().min(1).max(100).optional(),
  description: z.string().max(500).optional(),
  avatarUrl: z.string().url().nullable().optional(),
  settings: GroupSettingsSchema.partial().optional(),
});

export type UpdateGroupRequest = z.infer<typeof UpdateGroupRequestSchema>;

// ============================================================================
// GROUP MEMBER ACTIONS
// ============================================================================

export const AddMembersRequestSchema = z.object({
  groupId: z.string().min(1),
  memberIds: z.array(z.string().min(1)).min(1),
});

export type AddMembersRequest = z.infer<typeof AddMembersRequestSchema>;

export const RemoveMemberRequestSchema = z.object({
  groupId: z.string().min(1),
  memberId: z.string().min(1),
});

export type RemoveMemberRequest = z.infer<typeof RemoveMemberRequestSchema>;

export const UpdateMemberRoleRequestSchema = z.object({
  groupId: z.string().min(1),
  memberId: z.string().min(1),
  role: GroupRoleSchema,
});

export type UpdateMemberRoleRequest = z.infer<typeof UpdateMemberRoleRequestSchema>;

// ============================================================================
// GROUP INVITE LINKS
// ============================================================================

export const GroupInviteLinkSchema = z.object({
  id: z.string().min(1),
  groupId: z.string().min(1),
  code: z.string().min(1),
  url: z.string().url(),
  createdBy: z.string().min(1),
  createdAt: z.string().datetime(),
  expiresAt: z.string().datetime().optional(),
  maxUses: z.number().int().positive().optional(),
  useCount: z.number().int().min(0),
  isRevoked: z.boolean(),
});

export type GroupInviteLink = z.infer<typeof GroupInviteLinkSchema>;

// ============================================================================
// GROUP JOIN REQUESTS (for approval-required groups)
// ============================================================================

export const GroupJoinRequestSchema = z.object({
  id: z.string().min(1),
  groupId: z.string().min(1),
  userId: z.string().min(1),
  message: z.string().max(200).optional(),
  status: z.enum(['pending', 'approved', 'rejected']),
  requestedAt: z.string().datetime(),
  reviewedBy: z.string().optional(),
  reviewedAt: z.string().datetime().optional(),
});

export type GroupJoinRequest = z.infer<typeof GroupJoinRequestSchema>;

// ============================================================================
// GROUP EVENTS (for activity feed)
// ============================================================================

export const GroupEventTypeSchema = z.enum([
  'created',
  'name_changed',
  'description_changed',
  'avatar_changed',
  'member_added',
  'member_removed',
  'member_left',
  'member_promoted',
  'member_demoted',
  'settings_changed',
  'invite_link_created',
  'invite_link_revoked',
]);
export type GroupEventType = z.infer<typeof GroupEventTypeSchema>;

export const GroupEventSchema = z.object({
  id: z.string().min(1),
  groupId: z.string().min(1),
  type: GroupEventTypeSchema,
  actorId: z.string().min(1),
  targetId: z.string().optional(), // For member events
  metadata: z.record(z.unknown()).optional(),
  timestamp: z.string().datetime(),
});

export type GroupEvent = z.infer<typeof GroupEventSchema>;
