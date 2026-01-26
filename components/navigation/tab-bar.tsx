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
        'flex items-center p-1.5 gap-1',
        'rounded-[2.5rem] bg-foreground/10',
        'backdrop-blur-2xl border border-foreground/5',
        'shadow-[0_20px_50px_rgba(0,0,0,0.2)]',
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
              'relative flex flex-col items-center justify-center min-w-[72px] px-2 py-2.5 rounded-[2rem]',
              'transition-colors duration-300 outline-none select-none',
              isActive ? 'text-foreground' : 'text-muted-foreground hover:text-foreground/60'
            )}
            aria-label={tab.label}
          >
            {/* Liquid Glass Pill */}
            {isActive && (
              <motion.div
                layoutId="active-pill"
                className={cn(
                  'absolute inset-0 z-0 rounded-[2rem]',
                  'bg-foreground/[0.08]',
                  'backdrop-blur-md',
                  'border border-foreground/20',
                  'shadow-[inset_0_1px_1px_rgba(255,255,255,0.1),0_4px_12px_rgba(0,0,0,0.1)]',
                  'before:absolute before:inset-0 before:rounded-[2rem] before:bg-gradient-to-b before:from-foreground/10 before:to-transparent'
                )}
                transition={{
                  type: 'spring',
                  stiffness: 400,
                  damping: 30,
                  mass: 1
                }}
              />
            )}

            <div className="relative z-10 flex flex-col items-center gap-1">
              <div className="relative">
                <Icon 
                  className={cn(
                    'h-[22px] w-[22px] transition-transform duration-300',
                    isActive && 'scale-110'
                  )} 
                />
              </div>
              
              <span className={cn(
                'text-[10px] font-semibold tracking-wide uppercase',
                'transition-all duration-300',
                isActive ? 'opacity-100 scale-100' : 'opacity-40 scale-90'
              )}>
                {tab.label}
              </span>

              {/* Liquid Glow Dot */}
              {isActive && (
                <motion.div
                  layoutId="active-dot"
                  className="absolute -bottom-1 w-1 h-1 rounded-full bg-[#2AABEE] shadow-[0_0_8px_#2AABEE]"
                />
              )}
            </div>
          </button>
        );
      })}
    </nav>
  );
}
