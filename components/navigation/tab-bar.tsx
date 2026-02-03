'use client';

import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { MessageSquare, Circle, Phone, Settings } from 'lucide-react';
import { motion } from 'framer-motion';

interface TabBarProps {
  activeTab: TabType;
  onTabChange: (tab: TabType) => void;
}

export function TabBar({ activeTab, onTabChange }: TabBarProps) {
  const tabs: { id: TabType; label: string; icon: typeof MessageSquare }[] = [
    { id: 'chats', label: 'Chats', icon: MessageSquare },
    { id: 'status', label: 'Status', icon: Circle },
    { id: 'calls', label: 'Calls', icon: Phone },
    { id: 'settings', label: 'Settings', icon: Settings },
  ];

  return (
    <nav
      className={cn(
        'fixed bottom-8 left-1/2 -translate-x-1/2 z-50',
        'flex items-center p-2 gap-1',
        // iOS 26 style: cleaner white/dark glass with stronger blur
        'rounded-[32px]',
        'bg-white/85 dark:bg-black/70',
        'backdrop-blur-[32px]',
        'border border-black/[0.06] dark:border-white/[0.08]',
        // iOS 26 floating shadow
        'shadow-[0_8px_32px_rgba(0,0,0,0.12),0_2px_8px_rgba(0,0,0,0.08)]',
        'dark:shadow-[0_8px_32px_rgba(0,0,0,0.4),0_2px_8px_rgba(0,0,0,0.3)]',
        'safe-area-bottom'
      )}
    >
      {tabs.map((tab) => {
        const isActive = activeTab === tab.id;
        const Icon = tab.icon;

        return (
          <button
            key={tab.id}
            type="button"
            onClick={() => onTabChange(tab.id)}
            className={cn(
              'relative flex flex-col items-center justify-center min-w-[70px] px-3 py-2.5 rounded-[26px]',
              'transition-all duration-300 outline-none select-none',
              isActive
                ? 'text-foreground'
                : 'text-muted-foreground hover:text-foreground/70'
            )}
            aria-label={tab.label}
          >
            {/* iOS 26 Active Pill Indicator */}
            {isActive && (
              <motion.div
                layoutId="active-pill"
                className={cn(
                  'absolute inset-0 z-0 rounded-[26px]',
                  // iOS 26 subtle fill with depth
                  'bg-black/[0.05] dark:bg-white/[0.12]',
                  // Subtle inner shadow for 3D depth
                  'shadow-[inset_0_1px_0_rgba(255,255,255,0.5),0_2px_8px_rgba(0,0,0,0.06)]',
                  'dark:shadow-[inset_0_1px_0_rgba(255,255,255,0.1),0_2px_8px_rgba(0,0,0,0.2)]'
                )}
                transition={{
                  type: 'spring',
                  stiffness: 500,
                  damping: 35,
                  mass: 0.8
                }}
              />
            )}

            <div className="relative z-10 flex flex-col items-center gap-1">
              <div className="relative">
                <Icon
                  className={cn(
                    'h-[22px] w-[22px] transition-all duration-300',
                    isActive && 'scale-110'
                  )}
                  strokeWidth={isActive ? 2.2 : 1.8}
                />
              </div>

              <span className={cn(
                'text-[10px] font-semibold tracking-wide uppercase',
                'transition-all duration-300',
                isActive
                  ? 'opacity-100 scale-100'
                  : 'opacity-50 scale-95'
              )}>
                {tab.label}
              </span>
            </div>
          </button>
        );
      })}
    </nav>
  );
}
