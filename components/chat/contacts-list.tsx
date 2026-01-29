'use client';

import { useState, useEffect } from 'react';
import { Search, UserPlus } from 'lucide-react';
import { cn } from '@/lib/utils';
import * as supabaseUsers from '@/lib/supabase/users';
import type { User } from '@/lib/types';

interface ContactsListProps {
  onSelectUser: (userId: string) => void;
}

export function ContactsList({ onSelectUser }: ContactsListProps) {
  const [users, setUsers] = useState<any[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');

  useEffect(() => {
    const loadUsers = async () => {
      const allUsers = await supabaseUsers.getAllUsers();
      setUsers(allUsers);
      setIsLoading(false);
    };
    loadUsers();
  }, []);

  const filteredUsers = users.filter(user => 
    user.displayName?.toLowerCase().includes(searchQuery.toLowerCase()) ||
    user.username?.toLowerCase().includes(searchQuery.toLowerCase())
  );

  return (
    <div className="flex flex-col h-full bg-background">
      {/* Header */}
      <div className="px-4 py-4 flex items-center justify-between">
        <h1 className="text-2xl font-bold">Contacts</h1>
        <button className="p-2 rounded-full hover:bg-white/5 transition-colors">
          <UserPlus className="w-6 h-6 text-primary" />
        </button>
      </div>

      {/* Search */}
      <div className="px-4 mb-4">
        <div className="relative">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-muted-foreground" />
          <input
            type="text"
            placeholder="Search contacts"
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full bg-white/5 border-none rounded-xl py-2.5 pl-10 pr-4 text-sm focus:ring-1 focus:ring-primary outline-none transition-all"
          />
        </div>
      </div>

      {/* List */}
      <div className="flex-1 overflow-y-auto px-2">
        {isLoading ? (
          <div className="flex flex-col gap-4 p-4">
            {[1, 2, 3, 4, 5].map((i) => (
              <div key={i} className="flex items-center gap-3 animate-pulse">
                <div className="w-12 h-12 rounded-full bg-white/5" />
                <div className="flex-1 space-y-2">
                  <div className="h-4 bg-white/5 rounded w-1/3" />
                  <div className="h-3 bg-white/5 rounded w-1/2" />
                </div>
              </div>
            ))}
          </div>
        ) : (
          <div className="space-y-1">
            {filteredUsers.map((user) => (
              <button
                key={user.id}
                onClick={() => onSelectUser(user.id)}
                className="w-full flex items-center gap-3 p-3 rounded-xl hover:bg-white/5 transition-colors active:scale-[0.98]"
              >
                <div className="relative">
                  <img
                    src={user.avatar || 'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face'}
                    alt={user.displayName}
                    className="w-12 h-12 rounded-full object-cover"
                  />
                  {user.status === 'online' && (
                    <div className="absolute bottom-0 right-0 w-3 h-3 bg-green-500 border-2 border-background rounded-full" />
                  )}
                </div>
                <div className="flex-1 text-left">
                  <h3 className="font-semibold text-sm">{user.displayName}</h3>
                  <p className="text-xs text-muted-foreground truncate">{user.bio || 'Available'}</p>
                </div>
              </button>
            ))}
            {filteredUsers.length === 0 && (
              <div className="text-center py-10 text-muted-foreground">
                <p>No contacts found</p>
              </div>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
