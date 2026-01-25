import type { AuthContext } from '@/lib/platform/auth';
import { forbidden } from '@/lib/platform/auth';

// User privacy settings service.
// Enforces user preferences like read receipts, last seen visibility, etc.

export type Visibility = 'everyone' | 'contacts' | 'nobody';

export interface UserPrivacySettings {
  userId: string;
  readReceipts: boolean;
  lastSeenVisibility: Visibility;
  profilePhotoVisibility: Visibility;
  statusVisibility: Visibility;
  blockedUserIds: string[];
}

export interface PrivacyService {
  // Get user's privacy settings
  getSettings(userId: string): Promise<UserPrivacySettings | null>;
  
  // Check if viewer can see target's last seen
  canSeeLastSeen(viewerId: string, targetId: string): Promise<boolean>;
  
  // Check if read receipts should be sent
  shouldSendReadReceipt(senderId: string, recipientId: string): Promise<boolean>;
  
  // Check if user is blocked
  isBlocked(blockerId: string, blockedId: string): Promise<boolean>;
}

// Demo implementation with default settings
export const demoPrivacyService: PrivacyService = {
  async getSettings(userId: string): Promise<UserPrivacySettings | null> {
    // Return default permissive settings for demo
    return {
      userId,
      readReceipts: true,
      lastSeenVisibility: 'everyone',
      profilePhotoVisibility: 'everyone',
      statusVisibility: 'contacts',
      blockedUserIds: [],
    };
  },

  async canSeeLastSeen(viewerId: string, targetId: string): Promise<boolean> {
    const settings = await this.getSettings(targetId);
    if (!settings) return false;

    switch (settings.lastSeenVisibility) {
      case 'everyone':
        return true;
      case 'contacts':
        // TODO: Check if viewerId is in targetId's contacts
        return true; // Demo: assume everyone is a contact
      case 'nobody':
        return false;
    }
  },

  async shouldSendReadReceipt(senderId: string, recipientId: string): Promise<boolean> {
    // Both users must have read receipts enabled
    const senderSettings = await this.getSettings(senderId);
    const recipientSettings = await this.getSettings(recipientId);

    return (senderSettings?.readReceipts ?? true) && (recipientSettings?.readReceipts ?? true);
  },

  async isBlocked(blockerId: string, blockedId: string): Promise<boolean> {
    const settings = await this.getSettings(blockerId);
    return settings?.blockedUserIds.includes(blockedId) ?? false;
  },
};

/**
 * Check privacy rules before sending a read receipt.
 */
export async function canSendReadReceipt(
  privacyService: PrivacyService,
  senderId: string,
  recipientId: string
): Promise<boolean> {
  // Check if blocked
  const isBlocked = await privacyService.isBlocked(recipientId, senderId);
  if (isBlocked) return false;

  // Check read receipt settings
  return privacyService.shouldSendReadReceipt(senderId, recipientId);
}
