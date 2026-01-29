'use client';

import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { User, Phone, MessageCircle, Hexagon, Settings } from 'lucide-react';

interface TabBarProps {
  activeTab: TabType;
  onTabChange: (tab: TabType) => void;
}

export function TabBar({ activeTab, onTabChange }: TabBarProps) {
  const tabs: { id: TabType; label: string; icon: typeof MessageCircle }[] = [
    { id: 'contacts', label: 'Contacts', icon: User },
    { id: 'calls', label: 'Calls', icon: Phone },
    { id: 'chats', label: 'Chats', icon: MessageCircle },
    { id: 'node', label: 'Node', icon: Hexagon },
    { id: 'settings', label: 'Settings', icon: Settings },
  ];

  return (
    <nav
      className={cn(
        'fixed bottom-0 left-0 right-0 z-50',
        'flex items-center justify-around',
        'bg-background/95 backdrop-blur-lg',
        'border-t border-border',
        'px-2 pt-2 safe-area-bottom'
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
              'flex flex-col items-center justify-center flex-1 py-2 gap-1',
              'transition-colors duration-200 outline-none select-none',
              isActive ? 'text-primary' : 'text-muted-foreground'
            )}
            aria-label={tab.label}
          >
            <div className="relative">
              {/* Active indicator background for Node tab */}
              {isActive && tab.id === 'node' && (
                <div className="absolute -inset-1.5 bg-primary rounded-lg" />
              )}
              <Icon
                className={cn(
                  'h-6 w-6 relative z-10 transition-colors duration-200',
                  isActive && tab.id === 'node' && 'text-white'
                )}
              />
            </div>

            <span className={cn(
              'text-[10px] font-medium',
              isActive ? 'text-primary' : 'text-muted-foreground'
            )}>
              {tab.label}
            </span>
          </button>
        );
      })}
    </nav>
  );
}
