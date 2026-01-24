'use client';

import { useState } from 'react';
import { cn } from '@/lib/utils';
import type { TabType } from '@/lib/types';
import { chats, calls, getChatById } from '@/lib/mock-data';
import { TabBar } from './navigation/tab-bar';
import { ChatList } from './chat/chat-list';
import { ConversationView } from './chat/conversation-view';
import { StatusList } from './status/status-list';
import { CallsList } from './calls/calls-list';

export function AppShell() {
  const [activeTab, setActiveTab] = useState<TabType>('chats');
  const [selectedChatId, setSelectedChatId] = useState<string | null>(null);

  const selectedChat = selectedChatId ? getChatById(selectedChatId) : null;
  const unreadChats = chats.reduce((acc, chat) => acc + chat.unreadCount, 0);
  const missedCalls = calls.filter((call) => call.status === 'missed').length;

  const handleSelectChat = (chatId: string) => {
    setSelectedChatId(chatId);
  };

  const handleBackFromChat = () => {
    setSelectedChatId(null);
  };

  const renderContent = () => {
    // If a chat is selected, show the conversation view
    if (selectedChat && activeTab === 'chats') {
      return (
        <ConversationView chat={selectedChat} onBack={handleBackFromChat} />
      );
    }

    // Otherwise show the appropriate tab content
    switch (activeTab) {
      case 'chats':
        return (
          <ChatList
            selectedChatId={selectedChatId}
            onSelectChat={handleSelectChat}
          />
        );
      case 'status':
        return <StatusList />;
      case 'calls':
        return <CallsList />;
      default:
        return null;
    }
  };

  return (
    <div className="h-dvh flex flex-col overflow-hidden">
      {/* Main content area */}
      <main className={cn(
        'flex-1 overflow-hidden relative',
        selectedChat ? '' : ''
      )}>
        {renderContent()}
      </main>

      {/* Tab bar - hide when viewing a conversation */}
      {!selectedChat && (
        <TabBar
          activeTab={activeTab}
          onTabChange={setActiveTab}
          unreadChats={unreadChats}
          missedCalls={missedCalls}
        />
      )}
    </div>
  );
}
