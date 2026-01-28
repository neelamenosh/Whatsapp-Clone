'use client';

import { useState, useEffect, useCallback } from 'react';
import { cn } from '@/lib/utils';
import { CallItem } from './call-item';
import { Search, Phone, Video, Link2, Trash2, Loader2 } from 'lucide-react';
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
  const [subscriptionChannel, setSubscriptionChannel] = useState<RealtimeChannel | null>(null);

  // Initialize current user and fetch all users and call logs
  useEffect(() => {
    const user = getCurrentUser();
    setCurrentUser(user);

    const initializeData = async () => {
      // First, fetch all users
      const users = await getAllRegisteredUsers();
      setAllUsers(users);

      // Only fetch call logs after we have the users and currentUser
      if (user?.id) {
        setLoading(true);
        const { data, error } = await getCallLogsForUser(user.id);
        if (!error && data) {
          // Convert call logs to UI format using the fetched users
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

  // Helper function to convert CallLog to Call (not dependent on state)
  const convertLogToCall = (log: CallLog, userId: string, users: User[]): Call | null => {
    const isOutgoing = log.callerId === userId;
    const otherUserId = isOutgoing ? log.calleeId : log.callerId;

    // Find the other user's info
    const otherUser = users.find(u => u.id === otherUserId);

    // Map database status to UI status
    let uiStatus: Call['status'];
    if (log.status === 'outgoing') {
      uiStatus = 'outgoing';
    } else if (log.status === 'incoming' || log.status === 'completed') {
      uiStatus = isOutgoing ? 'outgoing' : 'incoming';
    } else if (log.status === 'missed') {
      uiStatus = 'missed';
    } else if (log.status === 'declined') {
      uiStatus = 'declined';
    } else {
      uiStatus = isOutgoing ? 'outgoing' : 'incoming';
    }

    const participant: User = otherUser || {
      id: otherUserId,
      name: 'Unknown',
      avatar: '/placeholder.svg',
      status: 'offline' as const,
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

  // Callback version for real-time updates (uses state)
  const callLogToCall = useCallback((log: CallLog, userId: string): Call | null => {
    return convertLogToCall(log, userId, allUsers);
  }, [allUsers]);

  // Subscribe to real-time updates
  useEffect(() => {
    if (!currentUser?.id) return;

    const channel = subscribeToCallLogs(
      currentUser.id,
      (newLog) => {
        const newCall = callLogToCall(newLog, currentUser.id);
        if (newCall) {
          setCalls(prev => [newCall, ...prev.filter(c => c.id !== newCall.id)]);
        }
      },
      (updatedLog) => {
        const updatedCall = callLogToCall(updatedLog, currentUser.id);
        if (updatedCall) {
          setCalls(prev => prev.map(c => c.id === updatedCall.id ? updatedCall : c));
        }
      }
    );

    setSubscriptionChannel(channel);

    return () => {
      if (channel) {
        unsubscribeFromCallLogs(channel);
      }
    };
  }, [currentUser?.id, callLogToCall]);

  const filteredCalls = calls.filter((call) => {
    const matchesFilter = filter === 'all' || call.status === 'missed';
    const matchesSearch = call.participants.some((p) =>
      p.name.toLowerCase().includes(searchQuery.toLowerCase())
    );
    return matchesFilter && matchesSearch;
  });

  return (
    <div className="flex flex-col h-full">
      {/* Header */}
      <div className="p-4 pb-2">
        <div className="flex items-center justify-between mb-4">
          <h1 className="text-2xl font-bold text-foreground">Calls</h1>
          <div className="flex items-center gap-2">
            <button
              type="button"
              className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-foreground"
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
                    if (!error) {
                      setCalls([]);
                    }
                  }
                }}
                className="glass-button w-10 h-10 flex items-center justify-center text-muted-foreground hover:text-destructive"
                aria-label="Clear call history"
              >
                <Trash2 className="h-5 w-5" />
              </button>
            )}
          </div>
        </div>

        {/* Search */}
        <div className="relative mb-4">
          <Search className="absolute left-4 top-1/2 -translate-y-1/2 h-5 w-5 text-muted-foreground" />
          <input
            type="text"
            placeholder="Search calls..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full glass-input pl-12 pr-4 py-3 text-sm text-foreground placeholder:text-muted-foreground"
          />
        </div>

        {/* Filter tabs */}
        <div className="flex gap-2">
          <button
            type="button"
            onClick={() => setFilter('all')}
            className={cn(
              'px-4 py-2 rounded-full text-sm font-medium transition-all duration-200',
              filter === 'all'
                ? 'bg-primary text-primary-foreground shadow-md shadow-primary/30'
                : 'glass-card text-muted-foreground hover:text-foreground'
            )}
          >
            All
          </button>
          <button
            type="button"
            onClick={() => setFilter('missed')}
            className={cn(
              'px-4 py-2 rounded-full text-sm font-medium transition-all duration-200',
              filter === 'missed'
                ? 'bg-primary text-primary-foreground shadow-md shadow-primary/30'
                : 'glass-card text-muted-foreground hover:text-foreground'
            )}
          >
            Missed
          </button>
        </div>
      </div>

      {/* Calls List */}
      <div className="flex-1 overflow-y-auto px-2 pb-4 scrollbar-hide">
        {/* Quick Actions */}
        <div className="px-2 py-4">
          <div className="grid grid-cols-2 gap-3">
            <button
              type="button"
              className="glass-card p-4 flex flex-col items-center gap-2 hover:scale-[1.02] transition-transform"
            >
              <div className="w-12 h-12 rounded-full bg-primary/10 flex items-center justify-center">
                <Phone className="h-6 w-6 text-primary" />
              </div>
              <span className="text-sm font-medium text-foreground">New Call</span>
            </button>
            <button
              type="button"
              className="glass-card p-4 flex flex-col items-center gap-2 hover:scale-[1.02] transition-transform"
            >
              <div className="w-12 h-12 rounded-full bg-primary/10 flex items-center justify-center">
                <Video className="h-6 w-6 text-primary" />
              </div>
              <span className="text-sm font-medium text-foreground">Video Call</span>
            </button>
          </div>
        </div>

        {/* Recent calls label */}
        <p className="px-4 py-2 text-xs font-medium text-muted-foreground uppercase tracking-wider">
          Recent
        </p>

        {/* Loading state */}
        {loading ? (
          <div className="flex flex-col items-center justify-center py-12">
            <Loader2 className="h-8 w-8 text-primary animate-spin" />
            <p className="text-muted-foreground mt-4">Loading calls...</p>
          </div>
        ) : filteredCalls.length > 0 ? (
          filteredCalls.map((call) => (
            <CallItem key={call.id} call={call} />
          ))
        ) : (
          <div className="flex flex-col items-center justify-center py-12 text-center">
            <div className="w-16 h-16 rounded-full bg-muted/50 flex items-center justify-center mb-4">
              <Phone className="h-8 w-8 text-muted-foreground" />
            </div>
            <p className="text-muted-foreground">
              {filter === 'missed' ? 'No missed calls' : 'No calls yet'}
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
