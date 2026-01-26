'use client';

import * as React from 'react';
import { useRouter } from 'next/navigation';
import { X, LogOut, User as UserIcon, ChevronRight, Trash2 } from 'lucide-react';
import { cn } from '@/lib/utils';
import { users, currentUser } from '@/lib/mock-data';
import { useSettings } from '@/components/settings-provider';
import { clearCurrentUser, getCurrentUser, deleteUserAccount } from '@/lib/auth-store';
import { ProfileModal } from '@/components/profile/profile-modal';
import type { Visibility } from '@/lib/settings';
import type { User } from '@/lib/types';

type SettingsModalProps = {
  open: boolean;
  onOpenChange: (open: boolean) => void;
};

function Section({ title, children }: { title: string; children: React.ReactNode }) {
  return (
    <section className="glass-card p-4">
      <h2 className="text-sm font-semibold text-foreground mb-3">{title}</h2>
      <div className="space-y-3">{children}</div>
    </section>
  );
}

function Row({
  label,
  description,
  right,
}: {
  label: string;
  description?: string;
  right: React.ReactNode;
}) {
  return (
    <div className="flex items-start justify-between gap-4">
      <div className="min-w-0">
        <p className="text-sm font-medium text-foreground">{label}</p>
        {description ? (
          <p className="text-xs text-muted-foreground mt-0.5">{description}</p>
        ) : null}
      </div>
      <div className="shrink-0">{right}</div>
    </div>
  );
}

function Toggle({
  checked,
  onChange,
  ariaLabel,
}: {
  checked: boolean;
  onChange: (checked: boolean) => void;
  ariaLabel: string;
}) {
  return (
    <button
      type="button"
      role="switch"
      aria-checked={checked}
      aria-label={ariaLabel}
      onClick={() => onChange(!checked)}
      className={cn(
        'relative w-12 h-7 rounded-full border transition-colors',
        checked ? 'bg-primary border-primary' : 'bg-muted/60 border-border'
      )}
    >
      <span
        className={cn(
          'absolute top-0.5 left-0.5 w-6 h-6 rounded-full bg-background shadow-sm transition-transform',
          checked ? 'translate-x-5' : 'translate-x-0'
        )}
      />
    </button>
  );
}

function VisibilitySelect({
  value,
  onChange,
  ariaLabel,
}: {
  value: Visibility;
  onChange: (v: Visibility) => void;
  ariaLabel: string;
}) {
  return (
    <select
      aria-label={ariaLabel}
      value={value}
      onChange={(e) => onChange(e.target.value as Visibility)}
      className="glass-input px-3 py-2 text-sm text-foreground"
    >
      <option value="everyone">Everyone</option>
      <option value="contacts">My contacts</option>
      <option value="nobody">Nobody</option>
    </select>
  );
}

function DisappearingSelect({
  value,
  onChange,
}: {
  value: number | null;
  onChange: (v: number | null) => void;
}) {
  return (
    <select
      aria-label="Disappearing messages"
      value={value ?? ''}
      onChange={(e) => {
        const raw = e.target.value;
        onChange(raw === '' ? null : Number(raw));
      }}
      className="glass-input px-3 py-2 text-sm text-foreground"
    >
      <option value="">Off</option>
      <option value={60}>1 minute</option>
      <option value={3600}>1 hour</option>
      <option value={86400}>24 hours</option>
      <option value={604800}>7 days</option>
    </select>
  );
}

export function SettingsModal({ open, onOpenChange }: SettingsModalProps) {
  const { settings, updateSettings, resetSettings } = useSettings();
  const router = useRouter();
  const [showLogoutConfirm, setShowLogoutConfirm] = React.useState(false);
  const [showDeleteConfirm, setShowDeleteConfirm] = React.useState(false);
  const [isDeleting, setIsDeleting] = React.useState(false);
  const [isProfileOpen, setIsProfileOpen] = React.useState(false);
  const [loggedInUser, setLoggedInUser] = React.useState<User | null>(null);

  // Load current user
  React.useEffect(() => {
    if (open) {
      setLoggedInUser(getCurrentUser());
    }
  }, [open, isProfileOpen]);

  const handleLogout = () => {
    // Clear current user session
    clearCurrentUser();
    
    onOpenChange(false);
    router.push('/login');
  };

  const handleDeleteAccount = async () => {
    setIsDeleting(true);
    
    const result = await deleteUserAccount();
    
    if (result.error) {
      alert(`Failed to delete account: ${result.error}`);
      setIsDeleting(false);
      return;
    }
    
    onOpenChange(false);
    router.push('/login');
  };

  React.useEffect(() => {
    function onKeyDown(e: KeyboardEvent) {
      if (e.key === 'Escape') onOpenChange(false);
    }
    if (open) window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [open, onOpenChange]);

  if (!open) return null;

  const blockableUsers = users.filter((u) => u.id !== currentUser.id);
  const blocked = new Set(settings.privacy.blockedUserIds);

  return (
    <div className="fixed inset-0 z-[100]">
      <button
        type="button"
        className="absolute inset-0 bg-black/30"
        aria-label="Close settings"
        onClick={() => onOpenChange(false)}
      />

      <div className="absolute inset-x-0 top-0 bottom-0 p-4 flex justify-center">
        <div className="w-full max-w-lg h-full glass-panel rounded-3xl overflow-hidden flex flex-col">
          <div className="px-4 py-3 flex items-center justify-between border-b border-border/40">
            <h1 className="text-base font-semibold text-foreground">Settings</h1>
            <button
              type="button"
              className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
              onClick={() => onOpenChange(false)}
              aria-label="Close"
            >
              <X className="h-5 w-5" />
            </button>
          </div>

          <div className="flex-1 overflow-y-auto p-4 space-y-4 scrollbar-hide">            {/* Profile Section */}
            <button
              type="button"
              onClick={() => setIsProfileOpen(true)}
              className="w-full glass-card p-4 flex items-center gap-4 hover:bg-muted/30 transition-colors"
            >
              <div className="relative">
                <div className={cn(
                  'w-16 h-16 rounded-full overflow-hidden',
                  'ring-2 ring-glass-border/50'
                )}>
                  <img
                    src={loggedInUser?.avatar || "/placeholder.svg"}
                    alt={loggedInUser?.name || "Profile"}
                    className="w-full h-full object-cover"
                    crossOrigin="anonymous"
                  />
                </div>
                {loggedInUser?.status === 'online' && (
                  <div className="absolute bottom-0 right-0 w-4 h-4 rounded-full bg-online border-2 border-background" />
                )}
              </div>
              <div className="flex-1 text-left">
                <p className="font-semibold text-foreground text-lg">{loggedInUser?.name || 'User'}</p>
                <p className="text-sm text-muted-foreground">{loggedInUser?.about || 'Available'}</p>
              </div>
              <ChevronRight className="h-5 w-5 text-muted-foreground" />
            </button>
            <Section title="Privacy">
              <Row
                label="Last seen"
                description="Control who can see when you were last active."
                right={
                  <VisibilitySelect
                    value={settings.privacy.lastSeenVisibility}
                    onChange={(v) =>
                      updateSettings((s) => ({
                        ...s,
                        privacy: { ...s.privacy, lastSeenVisibility: v },
                      }))
                    }
                    ariaLabel="Last seen visibility"
                  />
                }
              />
              <Row
                label="Read receipts"
                description="If turned off, you won’t send read receipts."
                right={
                  <Toggle
                    checked={settings.privacy.readReceipts}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        privacy: { ...s.privacy, readReceipts: checked },
                      }))
                    }
                    ariaLabel="Toggle read receipts"
                  />
                }
              />
              <Row
                label="Profile photo"
                description="Choose who can see your profile photo."
                right={
                  <VisibilitySelect
                    value={settings.privacy.profilePhotoVisibility}
                    onChange={(v) =>
                      updateSettings((s) => ({
                        ...s,
                        privacy: { ...s.privacy, profilePhotoVisibility: v },
                      }))
                    }
                    ariaLabel="Profile photo visibility"
                  />
                }
              />
              <Row
                label="Status"
                description="Choose who can see your status updates."
                right={
                  <VisibilitySelect
                    value={settings.privacy.statusVisibility}
                    onChange={(v) =>
                      updateSettings((s) => ({
                        ...s,
                        privacy: { ...s.privacy, statusVisibility: v },
                      }))
                    }
                    ariaLabel="Status visibility"
                  />
                }
              />
              <Row
                label="Disappearing messages"
                description="Default timer for new chats on this device."
                right={
                  <DisappearingSelect
                    value={settings.privacy.disappearingMessagesSeconds}
                    onChange={(v) =>
                      updateSettings((s) => ({
                        ...s,
                        privacy: { ...s.privacy, disappearingMessagesSeconds: v },
                      }))
                    }
                  />
                }
              />

              <div className="pt-2">
                <p className="text-xs font-medium text-muted-foreground mb-2">
                  Blocked contacts
                </p>
                <div className="space-y-2">
                  {blockableUsers.map((u) => {
                    const isBlocked = blocked.has(u.id);
                    return (
                      <div
                        key={u.id}
                        className="flex items-center justify-between gap-3"
                      >
                        <div className="flex items-center gap-3 min-w-0">
                          <div className="w-9 h-9 rounded-full overflow-hidden ring-2 ring-glass-border/30 shrink-0">
                            <img
                              src={u.avatar}
                              alt={u.name}
                              className="w-full h-full object-cover"
                              crossOrigin="anonymous"
                            />
                          </div>
                          <div className="min-w-0">
                            <p className="text-sm text-foreground truncate">{u.name}</p>
                          </div>
                        </div>
                        <Toggle
                          checked={isBlocked}
                          onChange={(checked) =>
                            updateSettings((s) => ({
                              ...s,
                              privacy: {
                                ...s.privacy,
                                blockedUserIds: checked
                                  ? Array.from(new Set([...s.privacy.blockedUserIds, u.id]))
                                  : s.privacy.blockedUserIds.filter((id) => id !== u.id),
                              },
                            }))
                          }
                          ariaLabel={`Block ${u.name}`}
                        />
                      </div>
                    );
                  })}
                </div>
              </div>
            </Section>

            <Section title="Security">
              <Row
                label="Device authentication"
                description="Require device auth to open the app (web demo: local gate only)."
                right={
                  <Toggle
                    checked={settings.security.requireDeviceAuth}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        security: { ...s.security, requireDeviceAuth: checked },
                      }))
                    }
                    ariaLabel="Require device authentication"
                  />
                }
              />
              <Row
                label="Two-step verification"
                description="Enable 2FA (web demo: placeholder)."
                right={
                  <Toggle
                    checked={settings.security.twoFactorEnabled}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        security: { ...s.security, twoFactorEnabled: checked },
                      }))
                    }
                    ariaLabel="Enable two-factor authentication"
                  />
                }
              />
            </Section>

            <Section title="Accessibility">
              <Row
                label="High contrast"
                description="Boost contrast for text and borders."
                right={
                  <Toggle
                    checked={settings.accessibility.highContrast}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        accessibility: { ...s.accessibility, highContrast: checked },
                      }))
                    }
                    ariaLabel="High contrast"
                  />
                }
              />
              <Row
                label="Reduced transparency"
                description="Use solid backgrounds instead of blur."
                right={
                  <Toggle
                    checked={settings.accessibility.reducedTransparency}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        accessibility: {
                          ...s.accessibility,
                          reducedTransparency: checked,
                        },
                      }))
                    }
                    ariaLabel="Reduced transparency"
                  />
                }
              />
              <Row
                label="Reduced motion"
                description="Minimize animations and transitions."
                right={
                  <Toggle
                    checked={settings.accessibility.reducedMotion}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        accessibility: { ...s.accessibility, reducedMotion: checked },
                      }))
                    }
                    ariaLabel="Reduced motion"
                  />
                }
              />
            </Section>

            <Section title="Integrations (stubs)">
              <Row
                label="Cloud sync"
                description="Requires a backend + account sign-in."
                right={
                  <Toggle
                    checked={settings.integrations.enableCloudSync}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: { ...s.integrations, enableCloudSync: checked },
                      }))
                    }
                    ariaLabel="Enable cloud sync"
                  />
                }
              />
              <Row
                label="End-to-end encrypted backup"
                description="Stub (web demo)."
                right={
                  <Toggle
                    checked={settings.integrations.enableEndToEndEncryptedBackup}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: {
                          ...s.integrations,
                          enableEndToEndEncryptedBackup: checked,
                        },
                      }))
                    }
                    ariaLabel="Enable encrypted backup"
                  />
                }
              />
              <Row
                label="Apple Music sharing"
                description="iOS-only; stubbed here."
                right={
                  <Toggle
                    checked={settings.integrations.enableAppleMusicSharing}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: {
                          ...s.integrations,
                          enableAppleMusicSharing: checked,
                        },
                      }))
                    }
                    ariaLabel="Enable Apple Music sharing"
                  />
                }
              />
              <Row
                label="Maps location sharing"
                description="Web-friendly (can use browser geolocation later)."
                right={
                  <Toggle
                    checked={settings.integrations.enableMapsSharing}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: {
                          ...s.integrations,
                          enableMapsSharing: checked,
                        },
                      }))
                    }
                    ariaLabel="Enable maps sharing"
                  />
                }
              />
              <Row
                label="Contacts"
                description="Web stub (permissions vary by browser)."
                right={
                  <Toggle
                    checked={settings.integrations.enableContactsIntegration}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: {
                          ...s.integrations,
                          enableContactsIntegration: checked,
                        },
                      }))
                    }
                    ariaLabel="Enable contacts integration"
                  />
                }
              />
              <Row
                label="Camera / Photos"
                description="Web-friendly (MediaDevices/File input later)."
                right={
                  <Toggle
                    checked={settings.integrations.enableCameraPhotosAccess}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: {
                          ...s.integrations,
                          enableCameraPhotosAccess: checked,
                        },
                      }))
                    }
                    ariaLabel="Enable camera/photos"
                  />
                }
              />
              <Row
                label="Files"
                description="Web-friendly (file picker later)."
                right={
                  <Toggle
                    checked={settings.integrations.enableFilesIntegration}
                    onChange={(checked) =>
                      updateSettings((s) => ({
                        ...s,
                        integrations: { ...s.integrations, enableFilesIntegration: checked },
                      }))
                    }
                    ariaLabel="Enable files integration"
                  />
                }
              />
            </Section>

            {/* Logout Section */}
            <Section title="Account">
              <div className="space-y-3">
                {!showLogoutConfirm ? (
                  <button
                    type="button"
                    onClick={() => setShowLogoutConfirm(true)}
                    className="w-full flex items-center justify-center gap-2 px-4 py-3 bg-muted/50 hover:bg-muted text-foreground font-medium rounded-xl transition-colors"
                  >
                    <LogOut className="w-5 h-5" />
                    Log out
                  </button>
                ) : (
                  <div className="p-4 bg-muted/30 rounded-xl space-y-3">
                    <p className="text-sm text-foreground text-center">
                      Are you sure you want to log out?
                    </p>
                    <div className="flex gap-3">
                      <button
                        type="button"
                        onClick={() => setShowLogoutConfirm(false)}
                        className="flex-1 px-4 py-2 bg-muted hover:bg-muted/80 text-foreground text-sm font-medium rounded-lg transition-colors"
                      >
                        Cancel
                      </button>
                      <button
                        type="button"
                        onClick={handleLogout}
                        className="flex-1 px-4 py-2 bg-primary hover:bg-primary/90 text-primary-foreground text-sm font-medium rounded-lg transition-colors"
                      >
                        Log out
                      </button>
                    </div>
                  </div>
                )}

                <div className="h-px bg-border/50 my-2" />

                {!showDeleteConfirm ? (
                  <button
                    type="button"
                    onClick={() => setShowDeleteConfirm(true)}
                    className="w-full flex items-center justify-center gap-2 px-4 py-3 bg-destructive/10 hover:bg-destructive/20 text-destructive font-medium rounded-xl transition-colors"
                  >
                    <Trash2 className="w-5 h-5" />
                    Delete Account
                  </button>
                ) : (
                  <div className="p-4 bg-destructive/10 rounded-xl space-y-3">
                    <p className="text-sm text-foreground text-center font-medium">
                      ⚠️ Delete your account?
                    </p>
                    <p className="text-xs text-muted-foreground text-center">
                      This will permanently delete your account and all your messages. This action cannot be undone.
                    </p>
                    <div className="flex gap-3">
                      <button
                        type="button"
                        onClick={() => setShowDeleteConfirm(false)}
                        disabled={isDeleting}
                        className="flex-1 px-4 py-2 bg-muted hover:bg-muted/80 text-foreground text-sm font-medium rounded-lg transition-colors disabled:opacity-50"
                      >
                        Cancel
                      </button>
                      <button
                        type="button"
                        onClick={handleDeleteAccount}
                        disabled={isDeleting}
                        className="flex-1 px-4 py-2 bg-destructive hover:bg-destructive/90 text-destructive-foreground text-sm font-medium rounded-lg transition-colors disabled:opacity-50"
                      >
                        {isDeleting ? 'Deleting...' : 'Delete'}
                      </button>
                    </div>
                  </div>
                )}
              </div>
            </Section>

            <div className="flex items-center justify-between gap-3">
              <button
                type="button"
                className="glass-card px-4 py-2 text-sm text-foreground hover:bg-muted/30 transition-colors"
                onClick={resetSettings}
              >
                Reset to defaults
              </button>
              <button
                type="button"
                className="glass-card px-4 py-2 text-sm text-muted-foreground hover:text-foreground transition-colors"
                onClick={() => onOpenChange(false)}
              >
                Done
              </button>
            </div>
          </div>
        </div>
      </div>

      {/* Profile Modal */}
      <ProfileModal
        open={isProfileOpen}
        onOpenChange={setIsProfileOpen}
      />
    </div>
  );
}
