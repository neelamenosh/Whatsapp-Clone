'use client';

import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { MessageSquare, Circle, Phone } from 'lucide-react';
import { GlassEffect, GlassFilter } from '@/components/ui/liquid-glass';

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
    <nav className="sticky bottom-0 z-50 w-full safe-area-bottom">
      <GlassFilter />

      <div className="px-4 pb-3">
        <GlassEffect className="glass-panel mx-auto max-w-md rounded-[2.25rem] p-2">
          <div className="flex items-center justify-between gap-1">
        {tabs.map((tab) => {
          const isActive = activeTab === tab.id;
          const Icon = tab.icon;

          return (
            <button
              key={tab.id}
              type="button"
              onClick={() => onTabChange(tab.id)}
              className={cn(
                'relative flex flex-1 flex-col items-center justify-center gap-1 px-3 py-2.5 rounded-2xl transition-all duration-300',
                isActive 
                  ? 'text-primary' 
                  : 'text-muted-foreground hover:text-foreground'
              )}
              aria-label={tab.label}
              aria-current={isActive ? 'page' : undefined}
            >
              {/* Active indicator */}
              {isActive && (
                <div className="absolute inset-0 bg-primary/10 rounded-2xl animate-in fade-in-0 zoom-in-95 duration-200" />
              )}

              <div className="relative z-10">
                <Icon 
                  className={cn(
                    'h-6 w-6 transition-transform duration-200',
                    isActive && 'scale-110'
                  )} 
                  fill={isActive ? 'currentColor' : 'none'}
                  strokeWidth={isActive ? 1.5 : 2}
                />

                {/* Badge */}
                {tab.badge && tab.badge > 0 && (
                  <div className={cn(
                    'absolute -top-1 -right-1 min-w-[18px] h-[18px] px-1',
                    'rounded-full bg-primary text-primary-foreground',
                    'flex items-center justify-center',
                    'text-[10px] font-semibold',
                    'badge-glow'
                  )}>
                    {tab.badge > 99 ? '99+' : tab.badge}
                  </div>
                )}
              </div>

              <span className={cn(
                'text-xs font-medium relative z-10 transition-all duration-200',
                isActive && 'font-semibold'
              )}>
                {tab.label}
              </span>
            </button>
          );
        })}
          </div>
        </GlassEffect>
      </div>
    </nav>
  );
}
