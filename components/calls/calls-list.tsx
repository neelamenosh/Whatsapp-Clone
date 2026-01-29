'use client';

import { useState, useEffect, useCallback } from 'react';
import { cn } from '@/lib/utils';
import { CallItem } from './call-item';
import { Search, Phone, Video, Link2, Trash2, Loader2, Plus } from 'lucide-react';
import { getCallLogsForUser, subscribeToCallLogs, unsubscribeFromCallLogs, clearCallLogsForUser, type CallLog } from '@/lib/supabase/call-logs';
import { getCurrentUser, getAllRegisteredUsers } from '@/lib/auth-store';
import type { Call, User } from '@/lib/types';
import type { RealtimeChannel } from '@supabase/supabase-js';

type FilterType = 'all' | 'missed';

export function CallsList() {
  const [filter, setFilter] = useState<FilterType>('all');
  const [searchQuery, setSearchQuery] = useState('');
  const [calls, setCalls] = useState<Call[]>([]);
  const [loading, setLoading] = useState(true);
  const [currentUser, setCurrentUser] = useState<User | null>(null);
  const [allUsers, setAllUsers] = useState<User[]>([]);

  useEffect(() => {
    const user = getCurrentUser();
    setCurrentUser(user);

    const initializeData = async () => {
      const users = await getAllRegisteredUsers();
      setAllUsers(users);

      if (user?.id) {
        setLoading(true);
        const { data, error } = await getCallLogsForUser(user.id);
        if (!error && data) {
          const convertedCalls = data
            .map(log => convertLogToCall(log, user.id, users))
            .filter((call): call is Call => call !== null);
          setCalls(convertedCalls);
        }
        setLoading(false);
      } else {
        setLoading(false);
      }
    };
    initializeData();
  }, []);

  const convertLogToCall = (log: CallLog, userId: string, users: User[]): Call | null => {
    const isOutgoing = log.callerId === userId;
    const otherUserId = isOutgoing ? log.calleeId : log.callerId;
    const otherUser = users.find(u => u.id === otherUserId);

    let uiStatus: Call['status'];
    if (log.status === 'outgoing') uiStatus = 'outgoing';
    else if (log.status === 'incoming' || log.status === 'completed') uiStatus = isOutgoing ? 'outgoing' : 'incoming';
    else if (log.status === 'missed') uiStatus = 'missed';
    else uiStatus = isOutgoing ? 'outgoing' : 'incoming';

    const participant: User = otherUser || {
      id: otherUserId,
      name: 'Unknown',
      avatar: '/placeholder.svg',
      status: 'offline',
    };

    return {
      id: log.id,
      participants: [participant],
      type: log.callType === 'video' ? 'video' : 'voice',
      status: uiStatus,
      startedAt: log.startedAt,
      endedAt: log.endedAt,
      duration: log.duration,
    };
  };

  const filteredCalls = calls.filter((call) => {
    const matchesFilter = filter === 'all' || call.status === 'missed';
    const matchesSearch = call.participants.some((p) =>
      p.name.toLowerCase().includes(searchQuery.toLowerCase())
    );
    return matchesFilter && matchesSearch;
  });

  return (
    <div className="flex flex-col h-full bg-background animate-in fade-in duration-300">
      {/* Header */}
      <div className="p-4 pb-2">
        <div className="flex items-center justify-between mb-4">
          <h1 className="text-2xl font-bold text-foreground">Calls</h1>
          <div className="flex items-center gap-2">
            <button
              type="button"
              className="p-2 text-primary hover:bg-primary/10 rounded-full transition-colors"
              aria-label="Create call link"
            >
              <Link2 className="h-5 w-5" />
            </button>
            {calls.length > 0 && (
              <button
                type="button"
                onClick={async () => {
                  if (currentUser?.id && confirm('Clear all call history?')) {
                    const { error } = await clearCallLogsForUser(currentUser.id);
                    if (!error) setCalls([]);
                  }
                }}
                className="p-2 text-muted-foreground hover:bg-red-500/10 hover:text-red-500 rounded-full transition-colors"
                aria-label="Clear call history"
              >
                <Trash2 className="h-5 w-5" />
              </button>
            )}
          </div>
        </div>

        {/* Filter tabs */}
        <div className="flex gap-2 mb-4 bg-muted/30 p-1 rounded-xl">
          <button
            type="button"
            onClick={() => setFilter('all')}
            className={cn(
              'flex-1 py-1.5 rounded-lg text-sm font-semibold transition-all duration-200',
              filter === 'all'
                ? 'bg-background text-primary shadow-sm'
                : 'text-muted-foreground hover:text-foreground'
            )}
          >
            All
          </button>
          <button
            type="button"
            onClick={() => setFilter('missed')}
            className={cn(
              'flex-1 py-1.5 rounded-lg text-sm font-semibold transition-all duration-200',
              filter === 'missed'
                ? 'bg-background text-primary shadow-sm'
                : 'text-muted-foreground hover:text-foreground'
            )}
          >
            Missed
          </button>
        </div>

        {/* Search */}
        <div className="relative mb-2">
          <Search className="absolute left-4 top-1/2 -translate-y-1/2 h-5 w-5 text-muted-foreground" />
          <input
            type="text"
            placeholder="Search calls..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full bg-muted/30 border-none rounded-2xl pl-12 pr-4 py-3 text-sm text-foreground placeholder:text-muted-foreground focus:bg-muted/50 transition-colors"
          />
        </div>
      </div>

      {/* Calls List */}
      <div className="flex-1 overflow-y-auto px-4 pb-24 scrollbar-hide">
        {/* Loading state */}
        {loading ? (
          <div className="flex flex-col items-center justify-center py-12">
            <Loader2 className="h-8 w-8 text-primary animate-spin" />
          </div>
        ) : filteredCalls.length > 0 ? (
          <div className="space-y-1">
            <p className="py-2 text-[10px] font-bold text-muted-foreground uppercase tracking-widest">
              Recent
            </p>
            {filteredCalls.map((call) => (
              <CallItem key={call.id} call={call} />
            ))}
          </div>
        ) : (
          <div className="flex flex-col items-center justify-center py-20 text-center">
            <div className="w-16 h-16 rounded-full bg-muted/50 flex items-center justify-center mb-4">
              <Phone className="h-8 w-8 text-muted-foreground" />
            </div>
            <p className="text-muted-foreground text-sm">
              {filter === 'missed' ? 'No missed calls' : 'No calls yet'}
            </p>
          </div>
        )}
      </div>

      {/* Floating Action Button */}
      <button
        type="button"
        className="absolute bottom-6 right-6 w-14 h-14 bg-primary text-white rounded-full flex items-center justify-center shadow-lg shadow-primary/30 hover:scale-110 active:scale-95 transition-all z-10"
      >
        <Plus className="h-6 w-6" />
      </button>
    </div>
  );
}
