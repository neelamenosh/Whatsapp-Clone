'use client';

import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { MessageSquare, Circle, Phone, Settings } from 'lucide-react';
import { motion } from 'framer-motion';

interface TabBarProps {
  activeTab: TabType;
  onTabChange: (tab: TabType) => void;
  unreadChats?: number;
  missedCalls?: number;
}

export function TabBar({ activeTab, onTabChange, unreadChats = 0, missedCalls = 0 }: TabBarProps) {
  const tabs: { id: TabType; label: string; icon: typeof MessageSquare; badge?: number }[] = [
    { id: 'chats', label: 'Chats', icon: MessageSquare, badge: unreadChats },
    { id: 'status', label: 'Status', icon: Circle },
    { id: 'calls', label: 'Calls', icon: Phone, badge: missedCalls },
    { id: 'settings', label: 'Settings', icon: Settings },
  ];

  return (
    <nav 
      className={cn(
        'fixed bottom-8 left-1/2 -translate-x-1/2 z-50',
        'flex items-center p-1.5 gap-1',
        'rounded-[2.5rem] bg-[rgba(15,15,15,0.4)]',
        'backdrop-blur-2xl border border-white/5',
        'shadow-[0_20px_50px_rgba(0,0,0,0.5)]',
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
              isActive ? 'text-white' : 'text-white/40 hover:text-white/60'
            )}
            aria-label={tab.label}
          >
            {/* Liquid Glass Pill */}
            {isActive && (
              <motion.div
                layoutId="active-pill"
                className={cn(
                  'absolute inset-0 z-0 rounded-[2rem]',
                  'bg-white/[0.08]',
                  'backdrop-blur-md',
                  'border border-white/20',
                  'shadow-[inset_0_1px_1px_rgba(255,255,255,0.2),0_4px_12px_rgba(0,0,0,0.3)]',
                  'before:absolute before:inset-0 before:rounded-[2rem] before:bg-gradient-to-b before:from-white/10 before:to-transparent'
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
                
                {/* Badge */}
                {tab.badge && tab.badge > 0 && (
                  <motion.div 
                    initial={{ scale: 0 }}
                    animate={{ scale: 1 }}
                    className="absolute -top-1.5 -right-1.5 min-w-[18px] h-[18px] px-1 rounded-full bg-[#2AABEE] text-white flex items-center justify-center text-[10px] font-bold border-2 border-[rgba(20,20,20,0.8)] shadow-lg shadow-[#2AABEE]/20"
                  >
                    {tab.badge > 99 ? '99+' : tab.badge}
                  </motion.div>
                )}
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
