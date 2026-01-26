'use client';

import * as React from 'react';
import { useRouter } from 'next/navigation';
import { 
  X, LogOut, User as UserIcon, ChevronRight, Trash2, Shield, Lock, 
  Globe, Share2, Eye, Layout, Sliders, Database, Link, Smartphone, 
  UserMinus, Fingerprint, Key, RefreshCw, Layers, Zap, Cloud, Server,
  Compass, Users, Camera, FileText, Moon, Sun, Monitor
} from 'lucide-react';
import { useTheme } from 'next-themes';
import { cn } from '@/lib/utils';
import { useSettings } from '@/components/settings-provider';
import type { Visibility } from '@/lib/settings';
import type { User } from '@/lib/types';
import { 
  getCurrentUser, 
  clearCurrentUser, 
  deleteUserAccount,
  getAllRegisteredUsers
} from '@/lib/auth-store';
import { ProfileModal } from '@/components/profile/profile-modal';

export interface SettingsModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

function Section({ title, children, icon: Icon }: { title: string; children: React.ReactNode; icon?: any }) {
  return (
    <section className="bg-foreground/[0.03] backdrop-blur-xl border border-foreground/10 rounded-2xl overflow-hidden shadow-[0_8px_32px_rgba(0,0,0,0.1)]">
      <div className="px-4 py-3 bg-foreground/[0.05] border-b border-foreground/5 flex items-center gap-2">
        {Icon && <Icon className="w-4 h-4 text-primary" />}
        <h2 className="text-[11px] font-bold text-foreground/60 uppercase tracking-wider">{title}</h2>
      </div>
      <div className="p-4 space-y-4">{children}</div>
    </section>
  );
}

function Row({
  label,
  description,
  right,
  icon: Icon
}: {
  label: string;
  description?: string;
  right: React.ReactNode;
  icon?: any;
}) {
  return (
    <div className="flex items-start justify-between gap-4 group">
      <div className="flex gap-3 min-w-0">
        {Icon && (
          <div className="mt-0.5 p-2 rounded-lg bg-foreground/[0.05] text-muted-foreground group-hover:text-[#2AABEE] transition-colors">
            <Icon className="w-4 h-4" />
          </div>
        )}
        <div className="min-w-0">
          <p className="text-sm font-semibold text-foreground/90 group-hover:text-foreground transition-colors">{label}</p>
          {description ? (
            <p className="text-[11px] text-muted-foreground mt-0.5 leading-relaxed">{description}</p>
          ) : null}
        </div>
      </div>
      <div className="shrink-0 pt-0.5">{right}</div>
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
        checked ? 'bg-[#2AABEE] border-[#2AABEE]' : 'bg-foreground/5 border-foreground/10'
      )}
    >
      <span
        className={cn(
          'absolute top-0.5 left-0.5 w-6 h-6 rounded-full bg-white shadow-sm transition-transform',
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
  const { theme, setTheme } = useTheme();
  const router = useRouter();
  const [showLogoutConfirm, setShowLogoutConfirm] = React.useState(false);
  const [showDeleteConfirm, setShowDeleteConfirm] = React.useState(false);
  const [isDeleting, setIsDeleting] = React.useState(false);
  const [isProfileOpen, setIsProfileOpen] = React.useState(false);
  const [loggedInUser, setLoggedInUser] = React.useState<User | null>(null);
  const [otherUsers, setOtherUsers] = React.useState<User[]>([]);

  // Load current user and other users
  React.useEffect(() => {
    if (open) {
      const user = getCurrentUser();
      setLoggedInUser(user);
      
      getAllRegisteredUsers().then(users => {
        if (user) {
          setOtherUsers(users.filter(u => u.id !== user.id));
        } else {
          setOtherUsers(users);
        }
      });
    }
  }, [open, isProfileOpen]);

  const handleLogout = () => {
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

            <div className="flex-1 overflow-y-auto p-6 space-y-8 scrollbar-hide">
              {/* Profile Section */}
              <button
                type="button"
                onClick={() => setIsProfileOpen(true)}
                className="w-full relative group"
              >
                <div className="absolute -inset-1 bg-gradient-to-r from-[#2AABEE] to-[#2AABEE]/20 rounded-[2rem] blur opacity-10 group-hover:opacity-20 transition duration-500" />
                  <div className="relative glass-card p-5 flex items-center gap-5 hover:bg-foreground/[0.05] transition-all duration-300">
                    <div className="relative">
                      <div className={cn(
                        'w-20 h-20 rounded-full overflow-hidden',
                        'ring-2 ring-foreground/10 group-hover:ring-[#2AABEE]/40 transition-all duration-500'
                      )}>
                        <img
                          src={loggedInUser?.avatar || "/placeholder.svg"}
                          alt={loggedInUser?.name || "Profile"}
                          className="w-full h-full object-cover transition-transform duration-500 group-hover:scale-110"
                          crossOrigin="anonymous"
                        />
                      </div>
                      {loggedInUser?.status === 'online' && (
                        <div className="absolute bottom-1 right-1 w-5 h-5 rounded-full bg-online border-[3px] border-background shadow-lg" />
                      )}
                    </div>
                    <div className="flex-1 text-left min-w-0">
                      <p className="text-[11px] font-bold text-[#2AABEE] uppercase tracking-[0.2em] mb-1">Account Info</p>
                      <p className="text-xl font-bold text-foreground tracking-tight truncate">{loggedInUser?.name || 'User'}</p>
                      <p className="text-sm text-muted-foreground truncate mt-0.5">{loggedInUser?.about || 'Hey there! I am using WhatsApp.'}</p>
                    </div>
                    <div className="w-10 h-10 rounded-full bg-foreground/5 flex items-center justify-center group-hover:bg-[#2AABEE]/10 transition-colors">
                      <ChevronRight className="h-5 w-5 text-muted-foreground group-hover:text-[#2AABEE]" />
                    </div>
                  </div>

              </button>

              <div className="space-y-6">
                <Section title="Privacy" icon={Shield}>
                  <Row
                    label="Last Seen"
                    description="Control who can see when you were last online."
                    icon={Eye}
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
                    label="Read Receipts"
                    description="If turned off, you won't send or receive Read Receipts."
                    icon={RefreshCw}
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
                    label="Profile Photo"
                    description="Choose who can see your profile picture."
                    icon={UserIcon}
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
                    label="Status Updates"
                    description="Manage who can see your status updates."
                    icon={Globe}
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
                    label="Disappearing Messages"
                    description="Automatically delete new messages after a certain time."
                    icon={Zap}
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

                  <div className="pt-4 mt-4 border-t border-foreground/5">
                    <div className="flex items-center gap-2 mb-4">
                      <UserMinus className="w-3.5 h-3.5 text-muted-foreground/60" />
                      <p className="text-[10px] font-bold text-muted-foreground/60 uppercase tracking-widest">
                        Blocked Contacts
                      </p>
                    </div>
                    <div className="grid grid-cols-1 gap-3">
                      {otherUsers.map((u) => {
                        const isBlocked = blocked.has(u.id);
                        return (
                          <div
                            key={u.id}
                            className="flex items-center justify-between p-3 rounded-xl bg-foreground/[0.02] border border-foreground/5 hover:bg-foreground/[0.04] transition-colors"
                          >
                            <div className="flex items-center gap-3 min-w-0">
                              <div className="w-8 h-8 rounded-full overflow-hidden ring-1 ring-foreground/10 shrink-0">
                                <img
                                  src={u.avatar}
                                  alt={u.name}
                                  className="w-full h-full object-cover"
                                  crossOrigin="anonymous"
                                />
                              </div>
                              <div className="min-w-0">
                                <p className="text-xs font-semibold text-foreground/80 truncate">{u.name}</p>
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

                <Section title="Security" icon={Lock}>
                  <Row
                    label="Device Lock"
                    description="Require biometric or passcode to unlock the app."
                    icon={Fingerprint}
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
                    label="Two-Step Verification"
                    description="Add an extra layer of security to your account."
                    icon={Key}
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

                <Section title="Appearance" icon={Layout}>
                  <Row
                    label="Theme"
                    description="Select your preferred theme mode."
                    icon={theme === 'dark' ? Moon : theme === 'light' ? Sun : Monitor}
                    right={
                      <div className="flex bg-foreground/5 rounded-xl p-1 border border-foreground/10">
                        <button
                          onClick={() => setTheme('light')}
                          className={cn(
                            "p-2 rounded-lg transition-all",
                            theme === 'light' ? "bg-background shadow-sm text-[#2AABEE]" : "text-muted-foreground hover:text-foreground"
                          )}
                          title="Light"
                        >
                          <Sun className="w-4 h-4" />
                        </button>
                        <button
                          onClick={() => setTheme('dark')}
                          className={cn(
                            "p-2 rounded-lg transition-all",
                            theme === 'dark' ? "bg-background shadow-sm text-[#2AABEE]" : "text-muted-foreground hover:text-foreground"
                          )}
                          title="Dark"
                        >
                          <Moon className="w-4 h-4" />
                        </button>
                        <button
                          onClick={() => setTheme('system')}
                          className={cn(
                            "p-2 rounded-lg transition-all",
                            theme === 'system' ? "bg-background shadow-sm text-[#2AABEE]" : "text-muted-foreground hover:text-foreground"
                          )}
                          title="System"
                        >
                          <Monitor className="w-4 h-4" />
                        </button>
                      </div>
                    }
                  />
                  <Row
                    label="High Contrast"
                    description="Increase the contrast for better visibility."
                    icon={Sliders}
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
                    label="Reduced Transparency"
                    description="Disable glass effects to improve performance."
                    icon={Layers}
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
                    label="Reduced Motion"
                    description="Minimize animations and motion effects."
                    icon={Zap}
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

                <Section title="App Settings" icon={Link}>
                  <Row
                    label="Cloud Sync"
                    description="Sync your data across all your devices."
                    icon={Cloud}
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
                    label="Chat Backups"
                    description="Manage your encrypted chat backups."
                    icon={Server}
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
                    label="Music Sharing"
                    description="Share your favorite music with contacts."
                    icon={Share2}
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
                        ariaLabel="Enable music sharing"
                      />
                    }
                  />
                  <Row
                    label="Maps & Location"
                    description="Use maps for sharing your real-time location."
                    icon={Compass}
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
                    label="Sync Contacts"
                    description="Connect with people from your phone's directory."
                    icon={Users}
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
                    label="Camera & Photos"
                    description="Access your camera and photo library."
                    icon={Camera}
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
                    label="Files & Documents"
                    description="Share files directly from your device storage."
                    icon={FileText}
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

                  <Section title="Account Management" icon={Database}>
                    <div className="space-y-4">
                      {!showLogoutConfirm ? (
                        <button
                          type="button"
                          onClick={() => setShowLogoutConfirm(true)}
                          className="w-full flex items-center justify-between p-4 bg-foreground/5 hover:bg-foreground/10 text-foreground font-semibold rounded-2xl transition-all duration-300 group"
                        >
                          <div className="flex items-center gap-3">
                            <div className="p-2 rounded-lg bg-foreground/5 text-muted-foreground group-hover:text-foreground transition-colors">
                              <LogOut className="w-4 h-4" />
                            </div>
                            <span>Logout</span>
                          </div>
                          <ChevronRight className="w-4 h-4 text-muted-foreground/40 group-hover:text-foreground" />
                        </button>
                      ) : (
                        <div className="p-5 glass-card border-foreground/10 space-y-4 animate-in fade-in slide-in-from-top-2 duration-300">
                          <p className="text-sm text-foreground/90 font-medium text-center">
                            Confirm Logout?
                          </p>
                          <div className="flex gap-3">
                            <button
                              type="button"
                              onClick={() => setShowLogoutConfirm(false)}
                              className="flex-1 px-4 py-2.5 bg-foreground/5 hover:bg-foreground/10 text-muted-foreground text-xs font-bold uppercase tracking-wider rounded-xl transition-colors"
                            >
                              Cancel
                            </button>
                            <button
                              type="button"
                              onClick={handleLogout}
                              className="flex-1 px-4 py-2.5 bg-[#2AABEE] hover:bg-[#2AABEE]/80 text-white text-xs font-bold uppercase tracking-wider rounded-xl transition-colors shadow-lg shadow-[#2AABEE]/20"
                            >
                              Logout
                            </button>
                          </div>
                        </div>
                      )}

                      {!showDeleteConfirm ? (
                        <button
                          type="button"
                          onClick={() => setShowDeleteConfirm(true)}
                          className="w-full flex items-center justify-between p-4 bg-red-500/5 hover:bg-red-500/10 text-red-500 font-semibold rounded-2xl transition-all duration-300 group border border-red-500/10"
                        >
                          <div className="flex items-center gap-3">
                            <div className="p-2 rounded-lg bg-red-500/5 text-red-500/40 group-hover:text-red-500 transition-colors">
                              <Trash2 className="w-4 h-4" />
                            </div>
                            <span>Delete My Account</span>
                          </div>
                          <ChevronRight className="w-4 h-4 text-red-500/20 group-hover:text-red-500" />
                        </button>
                      ) : (
                        <div className="p-5 glass-card border-red-500/20 space-y-4 animate-in fade-in slide-in-from-top-2 duration-300">
                          <div className="space-y-2 text-center">
                            <p className="text-sm text-red-500 font-bold uppercase tracking-wider">
                              WARNING
                            </p>
                            <p className="text-xs text-muted-foreground leading-relaxed">
                              This will permanently delete your account and all your messages. This action cannot be undone.
                            </p>
                          </div>
                          <div className="flex gap-3">
                            <button
                              type="button"
                              onClick={() => setShowDeleteConfirm(false)}
                              disabled={isDeleting}
                              className="flex-1 px-4 py-2.5 bg-foreground/5 hover:bg-foreground/10 text-muted-foreground text-xs font-bold uppercase tracking-wider rounded-xl transition-colors disabled:opacity-50"
                            >
                              Cancel
                            </button>
                            <button
                              type="button"
                              onClick={handleDeleteAccount}
                              disabled={isDeleting}
                              className="flex-1 px-4 py-2.5 bg-red-500 hover:bg-red-600 text-white text-xs font-bold uppercase tracking-wider rounded-xl transition-colors shadow-lg shadow-red-500/20 disabled:opacity-50"
                            >
                              {isDeleting ? 'Deleting...' : 'Delete Account'}
                            </button>
                          </div>
                        </div>
                      )}
                    </div>
                  </Section>

                  <div className="flex items-center gap-4 pt-4">
                    <button
                      type="button"
                      className="flex-1 glass-card px-4 py-3 text-[10px] font-bold uppercase tracking-[0.2em] text-muted-foreground hover:text-[#2AABEE] hover:bg-[#2AABEE]/5 transition-all duration-300 border border-foreground/5"
                      onClick={resetSettings}
                    >
                      Reset to Defaults
                    </button>
                    <button
                      type="button"
                      className="flex-1 glass-card px-4 py-3 text-[10px] font-bold uppercase tracking-[0.2em] text-[#2AABEE] hover:bg-[#2AABEE]/10 transition-all duration-300 border border-[#2AABEE]/20"
                      onClick={() => onOpenChange(false)}
                    >
                      Done
                    </button>
                  </div>

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
