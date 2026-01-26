'use client';

import * as React from 'react';
import { useRouter } from 'next/navigation';
import { 
  X, LogOut, User as UserIcon, ChevronRight, Trash2, Shield, Lock, 
  Globe, Share2, Eye, Layout, Sliders, Database, Link, Smartphone, 
  UserMinus, Fingerprint, Key, RefreshCw, Layers, Zap, Cloud, Server,
  Compass, Users, Camera, FileText
} from 'lucide-react';
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
    <section className="bg-white/[0.03] backdrop-blur-xl border border-white/10 rounded-2xl overflow-hidden shadow-[0_8px_32px_rgba(0,0,0,0.1)]">
      <div className="px-4 py-3 bg-white/[0.05] border-b border-white/5 flex items-center gap-2">
        {Icon && <Icon className="w-4 h-4 text-[#2AABEE]" />}
        <h2 className="text-[11px] font-bold text-white/50 uppercase tracking-wider">{title}</h2>
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
          <div className="mt-0.5 p-2 rounded-lg bg-white/[0.05] text-white/60 group-hover:text-[#2AABEE] transition-colors">
            <Icon className="w-4 h-4" />
          </div>
        )}
        <div className="min-w-0">
          <p className="text-sm font-semibold text-white/90 group-hover:text-white transition-colors">{label}</p>
          {description ? (
            <p className="text-[11px] text-white/40 mt-0.5 leading-relaxed">{description}</p>
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
        checked ? 'bg-[#2AABEE] border-[#2AABEE]' : 'bg-white/5 border-white/10'
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
              {/* Identity & Profile */}
              <button
                type="button"
                onClick={() => setIsProfileOpen(true)}
                className="w-full relative group"
              >
                <div className="absolute -inset-1 bg-gradient-to-r from-[#2AABEE] to-[#2AABEE]/20 rounded-[2rem] blur opacity-10 group-hover:opacity-20 transition duration-500" />
                <div className="relative glass-card p-5 flex items-center gap-5 hover:bg-white/[0.05] transition-all duration-300">
                  <div className="relative">
                    <div className={cn(
                      'w-20 h-20 rounded-full overflow-hidden',
                      'ring-2 ring-white/10 group-hover:ring-[#2AABEE]/40 transition-all duration-500'
                    )}>
                      <img
                        src={loggedInUser?.avatar || "/placeholder.svg"}
                        alt={loggedInUser?.name || "Profile"}
                        className="w-full h-full object-cover transition-transform duration-500 group-hover:scale-110"
                        crossOrigin="anonymous"
                      />
                    </div>
                    {loggedInUser?.status === 'online' && (
                      <div className="absolute bottom-1 right-1 w-5 h-5 rounded-full bg-online border-[3px] border-[#1a1a1a] shadow-lg" />
                    )}
                  </div>
                  <div className="flex-1 text-left min-w-0">
                    <p className="text-[11px] font-bold text-[#2AABEE] uppercase tracking-[0.2em] mb-1">Identity & Profile</p>
                    <p className="text-xl font-bold text-white tracking-tight truncate">{loggedInUser?.name || 'Authorized User'}</p>
                    <p className="text-sm text-white/40 truncate mt-0.5">{loggedInUser?.about || 'Status: Active'}</p>
                  </div>
                  <div className="w-10 h-10 rounded-full bg-white/5 flex items-center justify-center group-hover:bg-[#2AABEE]/10 transition-colors">
                    <ChevronRight className="h-5 w-5 text-white/20 group-hover:text-[#2AABEE]" />
                  </div>
                </div>
              </button>

              <div className="space-y-6">
                <Section title="Privacy & Data Governance" icon={Shield}>
                  <Row
                    label="Presence Management"
                    description="Configure visibility of real-time network presence and activity."
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
                        ariaLabel="Presence visibility"
                      />
                    }
                  />
                  <Row
                    label="Acknowledgment Receipts"
                    description="Enable or disable delivery and read confirmation protocols."
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
                    label="Asset Visibility"
                    description="Restrict profile image access to authorized contacts or groups."
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
                    label="Broadcast Presence"
                    description="Manage visibility of ephemeral status updates across the organization."
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
                    label="Data Retention Policy"
                    description="Define automatic expiration timers for peer-to-peer communications."
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

                  <div className="pt-4 mt-4 border-t border-white/5">
                    <div className="flex items-center gap-2 mb-4">
                      <UserMinus className="w-3.5 h-3.5 text-white/30" />
                      <p className="text-[10px] font-bold text-white/30 uppercase tracking-widest">
                        Access Restrictions
                      </p>
                    </div>
                    <div className="grid grid-cols-1 gap-3">
                      {otherUsers.map((u) => {
                        const isBlocked = blocked.has(u.id);
                        return (
                          <div
                            key={u.id}
                            className="flex items-center justify-between p-3 rounded-xl bg-white/[0.02] border border-white/5 hover:bg-white/[0.04] transition-colors"
                          >
                            <div className="flex items-center gap-3 min-w-0">
                              <div className="w-8 h-8 rounded-full overflow-hidden ring-1 ring-white/10 shrink-0">
                                <img
                                  src={u.avatar}
                                  alt={u.name}
                                  className="w-full h-full object-cover"
                                  crossOrigin="anonymous"
                                />
                              </div>
                              <div className="min-w-0">
                                <p className="text-xs font-semibold text-white/80 truncate">{u.name}</p>
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
                              ariaLabel={`Restrict ${u.name}`}
                            />
                          </div>
                        );
                      })}
                    </div>
                  </div>
                </Section>

                <Section title="Security & Authentication" icon={Lock}>
                  <Row
                    label="Identity Access Management (IAM)"
                    description="Mandate biometric or system-level authentication for application access."
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
                    label="Multi-Factor Authentication (MFA)"
                    description="Configure secondary verification layers for enhanced account security."
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

                <Section title="Interface & Accessibility" icon={Layout}>
                  <Row
                    label="High Contrast Mode"
                    description="Enhance visual legibility for high-compliance accessibility standards."
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
                    label="Transparency Protocols"
                    description="Optimize system performance by disabling complex glassmorphic effects."
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
                    label="Motion Optimization"
                    description="Minimize kinetic transitions to reduce cognitive load and energy consumption."
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

                <Section title="Enterprise Integrations" icon={Link}>
                  <Row
                    label="Data Synchronization"
                    description="Real-time syncing across enterprise nodes and authorized devices."
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
                    label="Encrypted Archives"
                    description="Manage end-to-end encrypted backup protocols for regulatory compliance."
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
                    label="External Media Services"
                    description="Sync with corporate media libraries and streaming services."
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
                        ariaLabel="Enable Apple Music sharing"
                      />
                    }
                  />
                  <Row
                    label="Geolocation Infrastructure"
                    description="Utilize GIS data for real-time location and asset tracking."
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
                    label="Directory Services"
                    description="Integrate with global contact directories and LDAP systems."
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
                    label="Visual Capture Access"
                    description="Permissions for integrated imaging hardware and secure photo libraries."
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
                    label="FileSystem Connectivity"
                    description="Direct interface with local and network-attached storage systems."
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

                <Section title="System & Compliance" icon={Database}>
                  <div className="space-y-4">
                    {!showLogoutConfirm ? (
                      <button
                        type="button"
                        onClick={() => setShowLogoutConfirm(true)}
                        className="w-full flex items-center justify-between p-4 bg-white/5 hover:bg-white/10 text-white font-semibold rounded-2xl transition-all duration-300 group"
                      >
                        <div className="flex items-center gap-3">
                          <div className="p-2 rounded-lg bg-white/5 text-white/40 group-hover:text-white transition-colors">
                            <LogOut className="w-4 h-4" />
                          </div>
                          <span>Terminate Session</span>
                        </div>
                        <ChevronRight className="w-4 h-4 text-white/20 group-hover:text-white" />
                      </button>
                    ) : (
                      <div className="p-5 glass-card border-white/10 space-y-4 animate-in fade-in slide-in-from-top-2 duration-300">
                        <p className="text-sm text-white/90 font-medium text-center">
                          Confirm Secure Logout?
                        </p>
                        <div className="flex gap-3">
                          <button
                            type="button"
                            onClick={() => setShowLogoutConfirm(false)}
                            className="flex-1 px-4 py-2.5 bg-white/5 hover:bg-white/10 text-white/60 text-xs font-bold uppercase tracking-wider rounded-xl transition-colors"
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
                        className="w-full flex items-center justify-between p-4 bg-red-500/5 hover:bg-red-500/10 text-red-400 font-semibold rounded-2xl transition-all duration-300 group border border-red-500/10"
                      >
                        <div className="flex items-center gap-3">
                          <div className="p-2 rounded-lg bg-red-500/5 text-red-500/40 group-hover:text-red-400 transition-colors">
                            <Trash2 className="w-4 h-4" />
                          </div>
                          <span>Purge User Data & Account</span>
                        </div>
                        <ChevronRight className="w-4 h-4 text-red-500/20 group-hover:text-red-400" />
                      </button>
                    ) : (
                      <div className="p-5 glass-card border-red-500/20 space-y-4 animate-in fade-in slide-in-from-top-2 duration-300">
                        <div className="space-y-2 text-center">
                          <p className="text-sm text-red-400 font-bold uppercase tracking-wider">
                            CRITICAL ACTION
                          </p>
                          <p className="text-xs text-white/40 leading-relaxed">
                            This will initiate a complete data purge from all distributed nodes. This action is irreversible and compliant with GDPR Right to Erasure.
                          </p>
                        </div>
                        <div className="flex gap-3">
                          <button
                            type="button"
                            onClick={() => setShowDeleteConfirm(false)}
                            disabled={isDeleting}
                            className="flex-1 px-4 py-2.5 bg-white/5 hover:bg-white/10 text-white/60 text-xs font-bold uppercase tracking-wider rounded-xl transition-colors disabled:opacity-50"
                          >
                            Abort
                          </button>
                          <button
                            type="button"
                            onClick={handleDeleteAccount}
                            disabled={isDeleting}
                            className="flex-1 px-4 py-2.5 bg-red-500 hover:bg-red-600 text-white text-xs font-bold uppercase tracking-wider rounded-xl transition-colors shadow-lg shadow-red-500/20 disabled:opacity-50"
                          >
                            {isDeleting ? 'PURGING...' : 'CONFIRM PURGE'}
                          </button>
                        </div>
                      </div>
                    )}
                  </div>
                </Section>

                <div className="flex items-center gap-4 pt-4">
                  <button
                    type="button"
                    className="flex-1 glass-card px-4 py-3 text-[10px] font-bold uppercase tracking-[0.2em] text-white/40 hover:text-[#2AABEE] hover:bg-[#2AABEE]/5 transition-all duration-300 border border-white/5"
                    onClick={resetSettings}
                  >
                    System Reset
                  </button>
                  <button
                    type="button"
                    className="flex-1 glass-card px-4 py-3 text-[10px] font-bold uppercase tracking-[0.2em] text-[#2AABEE] hover:bg-[#2AABEE]/10 transition-all duration-300 border border-[#2AABEE]/20"
                    onClick={() => onOpenChange(false)}
                  >
                    Commit Changes
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
