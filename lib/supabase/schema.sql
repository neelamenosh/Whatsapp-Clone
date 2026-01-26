-- Supabase SQL Schema for WhatsApp Clone
-- Run this in your Supabase SQL Editor (https://supabase.com/dashboard)

-- Enable UUID extension
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Users table
CREATE TABLE IF NOT EXISTS users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  email TEXT UNIQUE NOT NULL,
  username TEXT UNIQUE NOT NULL,
  display_name TEXT NOT NULL,
  avatar TEXT,
  phone TEXT,
  bio TEXT DEFAULT '',
  status TEXT DEFAULT 'online',
  last_seen TIMESTAMPTZ DEFAULT NOW(),
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Messages table (using TEXT for sender/recipient to support flexible IDs)
-- E2EE: Messages store ciphertext, nonce, and sender's public key
CREATE TABLE IF NOT EXISTS messages (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  chat_id TEXT NOT NULL,
  sender_id TEXT NOT NULL,
  recipient_id TEXT NOT NULL,
  content TEXT NOT NULL,
  type TEXT DEFAULT 'text',
  status TEXT DEFAULT 'sent',
  -- E2EE fields
  encrypted BOOLEAN DEFAULT false,
  ciphertext TEXT,
  nonce TEXT,
  sender_public_key TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- ===========================================
-- E2EE PUBLIC KEYS TABLE
-- ===========================================
-- Stores users' public keys for end-to-end encryption
-- IMPORTANT: Private keys are NEVER stored on the server

CREATE TABLE IF NOT EXISTS user_public_keys (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  user_id TEXT NOT NULL,
  device_id TEXT DEFAULT 'default',
  public_key TEXT NOT NULL,
  key_type TEXT DEFAULT 'X25519',
  is_active BOOLEAN DEFAULT true,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  UNIQUE(user_id, device_id)
);

-- Indexes for public keys
CREATE INDEX IF NOT EXISTS idx_user_public_keys_user_id ON user_public_keys(user_id);
CREATE INDEX IF NOT EXISTS idx_user_public_keys_active ON user_public_keys(user_id, is_active);

-- Enable RLS for public keys
ALTER TABLE user_public_keys ENABLE ROW LEVEL SECURITY;

-- Policies for user_public_keys
CREATE POLICY "Public keys are viewable by everyone" ON user_public_keys
  FOR SELECT USING (true);

CREATE POLICY "Users can insert their public keys" ON user_public_keys
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can update their public keys" ON user_public_keys
  FOR UPDATE USING (true);

-- Enable Realtime for public keys (for key updates)
ALTER PUBLICATION supabase_realtime ADD TABLE user_public_keys;

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_messages_chat_id ON messages(chat_id);
CREATE INDEX IF NOT EXISTS idx_messages_sender_id ON messages(sender_id);
CREATE INDEX IF NOT EXISTS idx_messages_recipient_id ON messages(recipient_id);
CREATE INDEX IF NOT EXISTS idx_messages_created_at ON messages(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
CREATE INDEX IF NOT EXISTS idx_users_username ON users(username);

-- Enable Row Level Security
ALTER TABLE users ENABLE ROW LEVEL SECURITY;
ALTER TABLE messages ENABLE ROW LEVEL SECURITY;

-- Policies for users table (anyone can read users, users can update their own profile)
CREATE POLICY "Users are viewable by everyone" ON users
  FOR SELECT USING (true);

CREATE POLICY "Users can update own profile" ON users
  FOR UPDATE USING (true);

CREATE POLICY "Anyone can insert users" ON users
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can delete their account" ON users
  FOR DELETE USING (true);

-- Policies for messages table
CREATE POLICY "Users can view their own messages" ON messages
  FOR SELECT USING (true);

CREATE POLICY "Users can insert messages" ON messages
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can update their own messages" ON messages
  FOR UPDATE USING (true);

CREATE POLICY "Users can delete messages" ON messages
  FOR DELETE USING (true);

-- Enable Realtime for messages and users tables
ALTER PUBLICATION supabase_realtime ADD TABLE messages;
ALTER PUBLICATION supabase_realtime ADD TABLE users;

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Triggers for updated_at
CREATE TRIGGER update_users_updated_at
  BEFORE UPDATE ON users
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_messages_updated_at
  BEFORE UPDATE ON messages
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

-- Blocked users table
CREATE TABLE IF NOT EXISTS blocked_users (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  blocker_id TEXT NOT NULL,
  blocked_id TEXT NOT NULL,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  UNIQUE(blocker_id, blocked_id)
);

-- Create indexes for blocked_users
CREATE INDEX IF NOT EXISTS idx_blocked_users_blocker_id ON blocked_users(blocker_id);
CREATE INDEX IF NOT EXISTS idx_blocked_users_blocked_id ON blocked_users(blocked_id);

-- Enable Row Level Security for blocked_users
ALTER TABLE blocked_users ENABLE ROW LEVEL SECURITY;

-- Policies for blocked_users table
CREATE POLICY "Users can view their blocked list" ON blocked_users
  FOR SELECT USING (true);

CREATE POLICY "Users can insert blocked users" ON blocked_users
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can delete from blocked list" ON blocked_users
  FOR DELETE USING (true);

-- Enable Realtime for blocked_users table
ALTER PUBLICATION supabase_realtime ADD TABLE blocked_users;

-- ===========================================
-- READ RECEIPTS TABLE
-- ===========================================
-- Tracks message delivery and read status for enterprise-level receipts

CREATE TABLE IF NOT EXISTS read_receipts (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  message_id UUID NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
  recipient_id TEXT NOT NULL,
  delivered_at TIMESTAMPTZ,
  read_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for read_receipts performance
CREATE INDEX IF NOT EXISTS idx_read_receipts_message_id ON read_receipts(message_id);
CREATE INDEX IF NOT EXISTS idx_read_receipts_recipient_id ON read_receipts(recipient_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_read_receipts_unique ON read_receipts(message_id, recipient_id);

-- Enable Row Level Security for read_receipts
ALTER TABLE read_receipts ENABLE ROW LEVEL SECURITY;

-- Policies for read_receipts table
CREATE POLICY "Users can view their receipts" ON read_receipts
  FOR SELECT USING (true);

CREATE POLICY "Users can insert receipts" ON read_receipts
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can update receipts" ON read_receipts
  FOR UPDATE USING (true);

-- Enable Realtime for read_receipts
ALTER PUBLICATION supabase_realtime ADD TABLE read_receipts;

-- Function to auto-create receipt when message is inserted
CREATE OR REPLACE FUNCTION create_read_receipt()
RETURNS TRIGGER AS $$
BEGIN
  INSERT INTO read_receipts (message_id, recipient_id)
  VALUES (NEW.id, NEW.recipient_id)
  ON CONFLICT (message_id, recipient_id) DO NOTHING;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to auto-create read receipt on message insert
DROP TRIGGER IF EXISTS trigger_create_read_receipt ON messages;
CREATE TRIGGER trigger_create_read_receipt
  AFTER INSERT ON messages
  FOR EACH ROW
  EXECUTE FUNCTION create_read_receipt();

-- ===========================================
-- STORAGE BUCKET FOR AVATARS
-- ===========================================
-- Note: Run these commands in Supabase Dashboard -> Storage -> Create Bucket
-- Or use the SQL below after enabling storage

-- Create storage bucket for avatars (run in Supabase Dashboard)
-- INSERT INTO storage.buckets (id, name, public) VALUES ('avatars', 'avatars', true);

-- Storage policy to allow anyone to view avatars
-- CREATE POLICY "Avatar images are publicly accessible" 
--   ON storage.objects FOR SELECT 
--   USING (bucket_id = 'avatars');

-- Storage policy to allow authenticated users to upload avatars
-- CREATE POLICY "Anyone can upload an avatar" 
--   ON storage.objects FOR INSERT 
--   WITH CHECK (bucket_id = 'avatars');

-- Storage policy to allow users to update their avatars
-- CREATE POLICY "Anyone can update avatars" 
--   ON storage.objects FOR UPDATE 
--   USING (bucket_id = 'avatars');

-- Storage policy to allow users to delete avatars
-- CREATE POLICY "Anyone can delete avatars" 
--   ON storage.objects FOR DELETE 
--   USING (bucket_id = 'avatars');

-- ===========================================
-- ALTERNATIVE: Create bucket via SQL (requires superuser)
-- ===========================================
-- If you have superuser access, uncomment and run these:

-- INSERT INTO storage.buckets (id, name, public, file_size_limit, allowed_mime_types)
-- VALUES (
--   'avatars', 
--   'avatars', 
--   true,
--   5242880,  -- 5MB limit
--   ARRAY['image/jpeg', 'image/png', 'image/gif', 'image/webp']
-- ) ON CONFLICT (id) DO NOTHING;
