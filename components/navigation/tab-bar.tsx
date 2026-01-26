'use client';

import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { MessageSquare, Circle, Phone, Users, Settings } from 'lucide-react';

interface TabBarProps {
  activeTab: TabType;
  onTabChange: (tab: TabType) => void;
  unreadChats?: number;
  missedCalls?: number;
}

export function TabBar({ activeTab, onTabChange, unreadChats = 0, missedCalls = 0 }: TabBarProps) {
  const tabs: { id: TabType; label: string; icon: typeof MessageSquare; badge?: number }[] = [
    { id: 'contacts', label: 'Contacts', icon: Users },
    { id: 'calls', label: 'Calls', icon: Phone, badge: missedCalls },
    { id: 'chats', label: 'Chats', icon: MessageSquare, badge: unreadChats },
    { id: 'settings', label: 'Settings', icon: Settings },
  ];

    return (
    <nav 
      className={cn(
        // Floating dock positioning
        'fixed bottom-6 left-1/2 -translate-x-1/2 z-50',
        'flex items-center gap-1',
        // Pill/capsule shape with smooth corners
        'rounded-[32px] px-3 py-2',
        // Dark glassmorphism background
        'bg-[rgba(28,28,30,0.78)]',
        'backdrop-blur-[30px] [-webkit-backdrop-filter:blur(30px)]',
        // Soft inner highlights and outer shadow for depth
        'shadow-[0_8px_32px_rgba(0,0,0,0.4),0_2px_8px_rgba(0,0,0,0.3)]',
        'border border-[rgba(255,255,255,0.08)]',
        // Inner glow highlight
        '[box-shadow:0_8px_32px_rgba(0,0,0,0.4),0_2px_8px_rgba(0,0,0,0.3),inset_0_1px_0_rgba(255,255,255,0.1),inset_0_-1px_0_rgba(0,0,0,0.2)]',
        // Safe area
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
                  'relative flex flex-col items-center gap-1.5 px-4 py-2 rounded-2xl',
                  // Smooth transitions
                  'transition-all duration-300',
                  // Color states
                  isActive 
                    ? 'text-white' 
                    : 'text-[rgba(255,255,255,0.4)] hover:text-[rgba(255,255,255,0.6)]',
                  'active:scale-95'
                )}
                aria-label={tab.label}
                aria-current={isActive ? 'page' : undefined}
              >
                <div className="relative">
                  {/* Active circular background */}
                  {isActive && (
                    <div 
                      className={cn(
                        'absolute inset-1/2 -translate-x-1/2 -translate-y-1/2 w-10 h-10 rounded-full',
                        'bg-[#2AABEE]',
                        'shadow-[0_0_15px_rgba(42,171,238,0.5)]',
                        'animate-in fade-in-0 zoom-in-75 duration-300'
                      )} 
                    />
                  )}

                  <div className="relative z-10 flex items-center justify-center w-10 h-10">
                    <Icon 
                      className={cn(
                        'h-6 w-6',
                        'transition-all duration-300',
                        isActive && 'scale-110'
                      )} 
                      strokeWidth={isActive ? 2 : 2}
                    />

                    {/* Badge */}
                    {tab.badge && tab.badge > 0 && (
                      <div className={cn(
                        'absolute top-0 -right-1 min-w-[18px] h-[18px] px-1',
                        'rounded-full bg-[#FF3B30] text-white',
                        'flex items-center justify-center',
                        'text-[10px] font-bold',
                        'border-2 border-[rgba(28,28,30,0.8)]'
                      )}>
                        {tab.badge > 99 ? '99+' : tab.badge}
                      </div>
                    )}
                  </div>
                </div>

                <span className={cn(
                  'text-[10px] font-medium relative z-10',
                  'transition-opacity duration-300',
                  isActive ? 'opacity-100' : 'opacity-60'
                )}>
                  {tab.label}
                </span>
              </button>
            );
          })}
    </nav>
  );
}
