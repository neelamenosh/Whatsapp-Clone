'use client';

import * as React from 'react';
import { cn } from '@/lib/utils';
import { getCurrentUser } from '@/lib/auth-store';
import type { User } from '@/lib/types';
import {
  ChevronLeft,
  ChevronDown,
  Pencil,
  Plus
} from 'lucide-react';

type ProfileModalProps = {
  open: boolean;
  onOpenChange: (open: boolean) => void;
};

export function ProfileModal({ open, onOpenChange }: ProfileModalProps) {
  const [user, setUser] = React.useState<User | null>(null);
  const [activeTab, setActiveTab] = React.useState<'main' | 'node'>('main');

  React.useEffect(() => {
    if (open) {
      const currentUser = getCurrentUser();
      setUser(currentUser);
    }
  }, [open]);

  if (!open || !user) return null;

  return (
    <div className="fixed inset-0 z-[110] bg-background animate-in slide-in-from-right duration-300">
      {/* Header */}
      <div className="px-4 py-4 flex items-center justify-between border-b border-border sticky top-0 bg-background z-10">
        <button
          onClick={() => onOpenChange(false)}
          className="flex items-center gap-1 text-primary font-medium"
        >
          <ChevronLeft className="w-5 h-5" />
          <span>Back</span>
        </button>
        <h2 className="text-sm font-bold">My Profile</h2>
        <button className="text-primary font-medium">Edit</button>
      </div>

      <div className="flex-1 overflow-y-auto scrollbar-hide pb-20">
        {/* Tabs */}
        <div className="flex border-b border-border">
          <button
            onClick={() => setActiveTab('main')}
            className={cn(
              "flex-1 py-3 text-sm font-semibold transition-colors relative",
              activeTab === 'main' ? "text-primary" : "text-muted-foreground"
            )}
          >
            Main Profile
            {activeTab === 'main' && (
              <div className="absolute bottom-0 left-0 right-0 h-0.5 bg-primary" />
            )}
          </button>
          <button
            onClick={() => setActiveTab('node')}
            className={cn(
              "flex-1 py-3 text-sm font-semibold transition-colors relative",
              activeTab === 'node' ? "text-primary" : "text-muted-foreground"
            )}
          >
            Profiles By Node
            {activeTab === 'node' && (
              <div className="absolute bottom-0 left-0 right-0 h-0.5 bg-primary" />
            )}
          </button>
        </div>

        {/* Node Selector */}
        <div className="p-4">
          <button className="flex items-center justify-between w-full p-4 bg-muted/30 rounded-2xl">
            <span className="font-semibold text-foreground">Mainnet Design</span>
            <ChevronDown className="w-5 h-5 text-muted-foreground" />
          </button>
        </div>

        {/* Profile Card */}
        <div className="px-4 mb-4">
          <div className="rounded-[2rem] overflow-hidden bg-gradient-to-b from-primary/80 to-primary/40 p-1 flex flex-col items-center pb-8 shadow-xl shadow-primary/10">
            <div className="relative mt-12 mb-4">
              <div className="w-32 h-32 rounded-full overflow-hidden border-4 border-white/20">
                <img
                  src={user.avatar || "/placeholder.svg"}
                  alt={user.name}
                  className="w-full h-full object-cover"
                  crossOrigin="anonymous"
                />
              </div>
              <button className="absolute bottom-0 right-0 w-10 h-10 bg-white rounded-full flex items-center justify-center shadow-lg">
                <Plus className="w-5 h-5 text-primary" />
              </button>
            </div>

            <h3 className="text-2xl font-bold text-white">{user.name}</h3>
            <p className="text-sm text-white/70 font-medium">@marcaum.eth</p>
          </div>
        </div>

        {/* Info Fields */}
        <div className="px-4 space-y-3">
          <div className="p-4 bg-muted/30 rounded-2xl flex items-center justify-between">
            <div className="flex flex-col">
              <span className="text-xs font-bold text-muted-foreground uppercase tracking-widest">name</span>
              <span className="text-foreground font-semibold mt-1">{user.name}</span>
            </div>
            <button className="p-2 text-primary">
              <Pencil className="w-4 h-4" />
            </button>
          </div>

          <div className="p-4 bg-muted/30 rounded-2xl">
            <span className="text-xs font-bold text-muted-foreground uppercase tracking-widest">about me</span>
            <p className="text-foreground font-medium mt-2 leading-relaxed">
              Hey there! I'm Marcaum. I love designing clean interfaces and building communities. 🚀
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
