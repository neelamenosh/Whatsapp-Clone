import { supabase, isSupabaseConfigured } from './client';

export interface User {
  id: string;
  email: string;
  username: string;
  displayName: string;
  avatar?: string;
  phone?: string;
  bio?: string;
  status?: 'online' | 'offline' | 'away';
  lastSeen?: string;
  createdAt?: string;
}

interface DbUser {
  id: string;
  email: string;
  username: string;
  display_name: string;
  avatar: string | null;
  phone: string | null;
  bio: string | null;
  status: string | null;
  last_seen: string | null;
  created_at: string;
}

function dbToUser(dbUser: DbUser): User {
  return {
    id: dbUser.id,
    email: dbUser.email,
    username: dbUser.username,
    displayName: dbUser.display_name,
    avatar: dbUser.avatar || undefined,
    phone: dbUser.phone || undefined,
    bio: dbUser.bio || undefined,
    status: (dbUser.status as User['status']) || 'offline',
    lastSeen: dbUser.last_seen || undefined,
    createdAt: dbUser.created_at,
  };
}

// Register a new user
export async function registerUser(
  email: string,
  username: string,
  displayName: string,
  password: string // Password stored as hash in real app, simplified for demo
): Promise<{ user: User | null; error: string | null }> {
  if (!isSupabaseConfigured() || !supabase) {
    return { user: null, error: 'Database not configured' };
  }

  try {
    // Check if email already exists
    const { data: existingEmail } = await supabase
      .from('users')
      .select('id')
      .eq('email', email.toLowerCase())
      .single();

    if (existingEmail) {
      return { user: null, error: 'Email already registered' };
    }

    // Check if username already exists
    const { data: existingUsername } = await supabase
      .from('users')
      .select('id')
      .eq('username', username.toLowerCase())
      .single();

    if (existingUsername) {
      return { user: null, error: 'Username already taken' };
    }

    // Create user
    const { data, error } = await supabase
      .from('users')
      .insert({
        email: email.toLowerCase(),
        username: username.toLowerCase(),
        display_name: displayName,
        status: 'online',
        last_seen: new Date().toISOString(),
      })
      .select()
      .single();

    if (error) {
      console.error('Registration error:', error);
      return { user: null, error: error.message };
    }

    return { user: dbToUser(data), error: null };
  } catch (err) {
    console.error('Registration error:', err);
    return { user: null, error: 'Registration failed' };
  }
}

// Login user
export async function loginUser(
  emailOrUsername: string,
  password: string // Simplified for demo
): Promise<{ user: User | null; error: string | null }> {
  if (!isSupabaseConfigured() || !supabase) {
    return { user: null, error: 'Database not configured' };
  }

  try {
    const identifier = emailOrUsername.toLowerCase();
    
    // Try to find user by email or username
    const { data, error } = await supabase
      .from('users')
      .select('*')
      .or(`email.eq.${identifier},username.eq.${identifier}`)
      .single();

    if (error || !data) {
      return { user: null, error: 'Invalid credentials' };
    }

    // Update status to online
    await supabase
      .from('users')
      .update({ status: 'online', last_seen: new Date().toISOString() })
      .eq('id', data.id);

    return { user: dbToUser(data), error: null };
  } catch (err) {
    console.error('Login error:', err);
    return { user: null, error: 'Login failed' };
  }
}

// Get user by ID
export async function getUserById(id: string): Promise<User | null> {
  if (!isSupabaseConfigured() || !supabase) return null;

  try {
    const { data, error } = await supabase
      .from('users')
      .select('*')
      .eq('id', id)
      .single();

    if (error || !data) return null;
    return dbToUser(data);
  } catch {
    return null;
  }
}

// Get all users (for chat list)
export async function getAllUsers(): Promise<User[]> {
  if (!isSupabaseConfigured() || !supabase) return [];

  try {
    const { data, error } = await supabase
      .from('users')
      .select('*')
      .order('display_name', { ascending: true });

    if (error || !data) return [];
    return data.map(dbToUser);
  } catch {
    return [];
  }
}

// Update user profile
export async function updateUserProfile(
  id: string,
  updates: Partial<Pick<User, 'displayName' | 'avatar' | 'phone' | 'bio' | 'status'>>
): Promise<{ user: User | null; error: string | null }> {
  if (!isSupabaseConfigured() || !supabase) {
    return { user: null, error: 'Database not configured' };
  }

  try {
    const dbUpdates: Record<string, any> = {};
    if (updates.displayName) dbUpdates.display_name = updates.displayName;
    if (updates.avatar !== undefined) dbUpdates.avatar = updates.avatar;
    if (updates.phone !== undefined) dbUpdates.phone = updates.phone;
    if (updates.bio !== undefined) dbUpdates.bio = updates.bio;
    if (updates.status) dbUpdates.status = updates.status;

    const { data, error } = await supabase
      .from('users')
      .update(dbUpdates)
      .eq('id', id)
      .select()
      .single();

    if (error) {
      return { user: null, error: error.message };
    }

    return { user: dbToUser(data), error: null };
  } catch (err) {
    return { user: null, error: 'Update failed' };
  }
}

// Update user online status
export async function updateUserStatus(id: string, status: 'online' | 'offline' | 'away'): Promise<void> {
  if (!isSupabaseConfigured() || !supabase) return;

  try {
    await supabase
      .from('users')
      .update({ 
        status, 
        last_seen: new Date().toISOString() 
      })
      .eq('id', id);
  } catch {
    // Silent fail for status updates
  }
}

// Subscribe to user status changes (real-time)
export function subscribeToUserStatus(
  userId: string,
  onStatusChange: (status: 'online' | 'offline' | 'away', lastSeen: string) => void
): (() => void) | null {
  if (!isSupabaseConfigured() || !supabase) return null;

  const channel = supabase
    .channel(`user-status:${userId}`)
    .on(
      'postgres_changes',
      {
        event: 'UPDATE',
        schema: 'public',
        table: 'users',
        filter: `id=eq.${userId}`,
      },
      (payload) => {
        const newData = payload.new as any;
        onStatusChange(
          newData.status || 'offline',
          newData.last_seen || new Date().toISOString()
        );
      }
    )
    .subscribe();

  return () => {
    if (supabase) supabase.removeChannel(channel);
  };
}

// Subscribe to all users status changes (for chat list)
export function subscribeToAllUsersStatus(
  onStatusChange: (userId: string, status: 'online' | 'offline' | 'away', lastSeen: string) => void
): (() => void) | null {
  if (!isSupabaseConfigured() || !supabase) return null;

  const channel = supabase
    .channel('all-users-status')
    .on(
      'postgres_changes',
      {
        event: 'UPDATE',
        schema: 'public',
        table: 'users',
      },
      (payload) => {
        const newData = payload.new as any;
        if (newData.id) {
          onStatusChange(
            newData.id,
            newData.status || 'offline',
            newData.last_seen || new Date().toISOString()
          );
        }
      }
    )
    .subscribe();

  return () => {
    if (supabase) supabase.removeChannel(channel);
  };
}

// Get user's current status from database
export async function getUserStatus(userId: string): Promise<{ status: 'online' | 'offline' | 'away'; lastSeen: string } | null> {
  if (!isSupabaseConfigured() || !supabase) return null;

  try {
    const { data, error } = await supabase
      .from('users')
      .select('status, last_seen')
      .eq('id', userId)
      .single();

    if (error || !data) return null;
    return {
      status: (data.status as 'online' | 'offline' | 'away') || 'offline',
      lastSeen: data.last_seen || new Date().toISOString(),
    };
  } catch {
    return null;
  }
}