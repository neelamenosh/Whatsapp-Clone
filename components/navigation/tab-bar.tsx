'use client';

import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { MessageSquare, Circle, Phone, Users } from 'lucide-react';

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
  ];

  return (
    <nav 
      className={cn(
        // Floating dock positioning
        'fixed bottom-6 left-1/2 -translate-x-1/2 z-50',
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
      <div className="flex items-center justify-around gap-1">
        {tabs.map((tab) => {
          const isActive = activeTab === tab.id;
          const Icon = tab.icon;

          return (
            <button
              key={tab.id}
              type="button"
              onClick={() => onTabChange(tab.id)}
              className={cn(
                'relative flex flex-col items-center gap-0.5 px-5 py-2 rounded-2xl',
                // Smooth transitions
                'transition-all duration-[280ms] ease-[cubic-bezier(0.25,0.1,0.25,1)]',
                // Color states
                isActive 
                  ? 'text-[#2AABEE]' 
                  : 'text-[rgba(255,255,255,0.45)] hover:text-[rgba(255,255,255,0.7)]',
                // Hover scale
                'hover:scale-[1.08] active:scale-[1.02]'
              )}
              aria-label={tab.label}
              aria-current={isActive ? 'page' : undefined}
            >
              {/* Active glow background */}
              {isActive && (
                <div 
                  className={cn(
                    'absolute inset-0 rounded-2xl',
                    'bg-[rgba(42,171,238,0.12)]',
                    'shadow-[0_0_20px_rgba(42,171,238,0.25)]',
                    'animate-in fade-in-0 zoom-in-95 duration-300'
                  )} 
                />
              )}

              <div className="relative z-10">
                <Icon 
                  className={cn(
                    'h-6 w-6',
                    'transition-all duration-[280ms] ease-[cubic-bezier(0.25,0.1,0.25,1)]',
                    // Active state: scale emphasis + glow
                    isActive && 'scale-110 drop-shadow-[0_0_8px_rgba(42,171,238,0.5)]'
                  )} 
                  fill={isActive ? 'currentColor' : 'none'}
                  strokeWidth={isActive ? 1.5 : 2}
                />

                {/* Badge */}
                {tab.badge && tab.badge > 0 && (
                  <div className={cn(
                    'absolute -top-1.5 -right-2 min-w-[18px] h-[18px] px-1',
                    'rounded-full bg-[#2AABEE] text-white',
                    'flex items-center justify-center',
                    'text-[10px] font-semibold',
                    'shadow-[0_0_12px_rgba(42,171,238,0.6)]',
                    'border border-[rgba(255,255,255,0.15)]'
                  )}>
                    {tab.badge > 99 ? '99+' : tab.badge}
                  </div>
                )}
              </div>

              <span className={cn(
                'text-[11px] font-medium relative z-10',
                'transition-all duration-[280ms] ease-[cubic-bezier(0.25,0.1,0.25,1)]',
                isActive && 'font-semibold'
              )}>
                {tab.label}
              </span>
            </button>
          );
        })}
      </div>
    </nav>
  );
}
