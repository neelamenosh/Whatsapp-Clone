'use client';

import * as React from 'react';
import {
    ChevronRight,
    Grid,
    Wallet,
    MessageSquare,
    Phone,
    Smartphone,
    FolderOpen,
    Settings as SettingsIcon,
    LogOut,
    User as UserIcon
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { getCurrentUser } from '@/lib/auth-store';
import { ProfileModal } from '@/components/profile/profile-modal';
import type { User } from '@/lib/types';

export function SettingsView() {
    const [loggedInUser, setLoggedInUser] = React.useState<User | null>(null);
    const [isProfileOpen, setIsProfileOpen] = React.useState(false);

    React.useEffect(() => {
        const user = getCurrentUser();
        setLoggedInUser(user);
    }, [isProfileOpen]);

    const MenuItem = ({ icon: Icon, label, color = "text-muted-foreground", badge }: { icon: any, label: string, color?: string, badge?: string | number }) => (
        <button
            type="button"
            className="list-item-card w-full"
            onClick={() => label === 'My Profile' && setIsProfileOpen(true)}
        >
            <div className="flex items-center gap-3">
                <div className={cn("p-2 rounded-lg bg-muted/50", color)}>
                    <Icon className="w-5 h-5" />
                </div>
                <span className="text-sm font-semibold text-foreground">{label}</span>
            </div>
            <div className="flex items-center gap-2">
                {badge && <span className="text-xs text-muted-foreground">{badge}</span>}
                <ChevronRight className="w-4 h-4 text-muted-foreground/40" />
            </div>
        </button>
    );

    return (
        <div className="flex flex-col h-full bg-background overflow-y-auto scrollbar-hide pb-24">
            {/* Header */}
            <div className="px-4 py-4 flex items-center justify-between sticky top-0 bg-background/80 backdrop-blur-md z-10">
                <button type="button" className="p-2 -ml-2 text-primary">
                    <Grid className="w-6 h-6" />
                </button>
                <button type="button" className="text-primary font-semibold text-sm">
                    Edit
                </button>
            </div>

            {/* Profile Section */}
            <div className="flex flex-col items-center px-4 py-6">
                <div className="relative mb-4">
                    <div className="w-24 h-24 rounded-full overflow-hidden ring-4 ring-primary/10">
                        <img
                            src={loggedInUser?.avatar || "/placeholder.svg"}
                            alt={loggedInUser?.name}
                            className="w-full h-full object-cover"
                            crossOrigin="anonymous"
                        />
                    </div>
                    <div className="absolute bottom-0 right-0 w-6 h-6 rounded-full bg-primary border-4 border-background" />
                </div>

                <h1 className="text-2xl font-bold text-foreground">{loggedInUser?.name || "User"}</h1>
                <p className="text-sm text-muted-foreground mt-1">@marcaum.eth • +1 (555) 000-0000</p>

                <button
                    type="button"
                    className="mt-6 pill-button"
                >
                    Change Profile Photo
                </button>
            </div>

            {/* Menu Sections */}
            <div className="px-4 space-y-4">
                <div className="space-y-1">
                    <MenuItem icon={UserIcon} label="My Profile" color="text-blue-500" />
                    <MenuItem icon={Wallet} label="Wallet" color="text-yellow-500" />
                    <MenuItem icon={MessageSquare} label="Saved Messages" color="text-blue-400" />
                    <MenuItem icon={Phone} label="Recent Calls" color="text-green-500" />
                    <MenuItem icon={Smartphone} label="Devices" color="text-orange-500" />
                    <MenuItem icon={FolderOpen} label="Chat Folders" color="text-blue-500" />
                </div>

                <div className="space-y-1">
                    <MenuItem icon={SettingsIcon} label="Settings" color="text-gray-500" />
                    <MenuItem icon={LogOut} label="Logout" color="text-red-500" />
                </div>
            </div>

            <ProfileModal
                open={isProfileOpen}
                onOpenChange={setIsProfileOpen}
            />
        </div>
    );
}
