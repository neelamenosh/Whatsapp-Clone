'use client';

import { useState, useEffect } from 'react';
import { Search, UserPlus, Loader2 } from 'lucide-react';
import { cn } from '@/lib/utils';
import * as supabaseUsers from '@/lib/supabase/users';

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
    <div className="flex flex-col h-full bg-background animate-in fade-in duration-300">
      {/* Header */}
      <div className="px-4 py-4 flex items-center justify-between">
        <h1 className="text-2xl font-bold text-foreground">Contacts</h1>
        <button className="p-2 rounded-full hover:bg-primary/10 transition-colors">
          <UserPlus className="w-6 h-6 text-primary" />
        </button>
      </div>

      {/* Search */}
      <div className="px-4 mb-4">
        <div className="relative">
          <Search className="absolute left-4 top-1/2 -translate-y-1/2 w-4 h-4 text-muted-foreground" />
          <input
            type="text"
            placeholder="Search contacts"
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full bg-muted/30 border-none rounded-2xl py-3 pl-11 pr-4 text-sm focus:bg-muted/50 outline-none transition-all"
          />
        </div>
      </div>

      {/* List */}
      <div className="flex-1 overflow-y-auto px-4 pb-24 scrollbar-hide">
        {isLoading ? (
          <div className="flex items-center justify-center py-12">
            <Loader2 className="h-8 w-8 text-primary animate-spin" />
          </div>
        ) : (
          <div className="space-y-1">
            <p className="py-2 text-[10px] font-bold text-muted-foreground uppercase tracking-widest">
              All Contacts
            </p>
            {filteredUsers.map((user) => (
              <button
                key={user.id}
                onClick={() => onSelectUser(user.id)}
                className="w-full flex items-center gap-3 p-3 rounded-2xl hover:bg-muted/50 transition-all active:scale-[0.98] text-left"
              >
                <div className="relative shrink-0">
                  <img
                    src={user.avatar || 'https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?w=100&h=100&fit=crop&crop=face'}
                    alt={user.displayName}
                    className="w-12 h-12 rounded-full object-cover"
                    crossOrigin="anonymous"
                  />
                  {user.status === 'online' && (
                    <div className="absolute bottom-0 right-0 w-3 h-3 bg-primary border-2 border-background rounded-full" />
                  )}
                </div>
                <div className="flex-1 min-w-0">
                  <h3 className="font-bold text-sm text-foreground truncate">{user.displayName}</h3>
                  <p className="text-xs text-muted-foreground truncate font-medium">
                    {user.bio || 'Available'}
                  </p>
                </div>
              </button>
            ))}
            {filteredUsers.length === 0 && (
              <div className="text-center py-20 text-muted-foreground">
                <p className="text-sm">No contacts found</p>
              </div>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
