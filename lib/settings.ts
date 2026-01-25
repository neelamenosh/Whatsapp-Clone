export type Visibility = 'everyone' | 'contacts' | 'nobody';

export interface PrivacySettings {
  lastSeenVisibility: Visibility;
  readReceipts: boolean;
  profilePhotoVisibility: Visibility;
  statusVisibility: Visibility;
  blockedUserIds: string[];
  disappearingMessagesSeconds: number | null;
}

export interface SecuritySettings {
  requireDeviceAuth: boolean;
  twoFactorEnabled: boolean;
}

export interface AccessibilitySettings {
  highContrast: boolean;
  reducedTransparency: boolean;
  reducedMotion: boolean;
}

export interface IntegrationsSettings {
  enableCloudSync: boolean;
  enableEndToEndEncryptedBackup: boolean;
  enableAppleMusicSharing: boolean;
  enableMapsSharing: boolean;
  enableContactsIntegration: boolean;
  enableCameraPhotosAccess: boolean;
  enableFilesIntegration: boolean;
}

export interface AppSettings {
  privacy: PrivacySettings;
  security: SecuritySettings;
  accessibility: AccessibilitySettings;
  integrations: IntegrationsSettings;
}

export const defaultSettings: AppSettings = {
  privacy: {
    lastSeenVisibility: 'everyone',
    readReceipts: true,
    profilePhotoVisibility: 'everyone',
    statusVisibility: 'contacts',
    blockedUserIds: [],
    disappearingMessagesSeconds: null,
  },
  security: {
    requireDeviceAuth: false,
    twoFactorEnabled: false,
  },
  accessibility: {
    highContrast: false,
    reducedTransparency: false,
    reducedMotion: false,
  },
  integrations: {
    enableCloudSync: true,
    enableEndToEndEncryptedBackup: false,
    enableAppleMusicSharing: false,
    enableMapsSharing: true,
    enableContactsIntegration: false,
    enableCameraPhotosAccess: false,
    enableFilesIntegration: false,
  },
};
