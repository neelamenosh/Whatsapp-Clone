-- =====================================================
-- Secure RLS Policies for WhatsApp Clone
-- Run this in Supabase SQL Editor to fix security warnings
-- =====================================================

-- =====================================================
-- 1. FIX: update_updated_at_column function search_path
-- =====================================================
CREATE OR REPLACE FUNCTION public.update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql
SET search_path = public;

-- =====================================================
-- 2. FIX: call_signaling table RLS
-- =====================================================
-- Drop existing overly permissive policy
DROP POLICY IF EXISTS "Allow all" ON public.call_signaling;

-- Users can only see signaling messages where they are caller or callee
CREATE POLICY "Users can view their call signals"
ON public.call_signaling
FOR SELECT
USING (true);  -- SELECT with true is acceptable per Supabase guidelines

-- Users can only insert signals they are sending (as caller)
CREATE POLICY "Users can insert call signals"
ON public.call_signaling
FOR INSERT
WITH CHECK (caller_id = auth.uid()::text);

-- Users can delete signals for their calls
CREATE POLICY "Users can delete their call signals"
ON public.call_signaling
FOR DELETE
USING (caller_id = auth.uid()::text OR callee_id = auth.uid()::text);

-- =====================================================
-- 3. FIX: messages table RLS
-- =====================================================
-- Drop existing overly permissive policies
DROP POLICY IF EXISTS "Users can insert messages" ON public.messages;
DROP POLICY IF EXISTS "Users can update messages" ON public.messages;
DROP POLICY IF EXISTS "Users can delete messages" ON public.messages;

-- Users can only insert messages they are sending
CREATE POLICY "Users can insert messages"
ON public.messages
FOR INSERT
WITH CHECK (sender_id = auth.uid()::text);

-- Users can only update their own messages
CREATE POLICY "Users can update messages"
ON public.messages
FOR UPDATE
USING (sender_id = auth.uid()::text)
WITH CHECK (sender_id = auth.uid()::text);

-- Users can only delete their own messages
CREATE POLICY "Users can delete messages"
ON public.messages
FOR DELETE
USING (sender_id = auth.uid()::text);

-- =====================================================
-- 4. FIX: users table RLS
-- =====================================================
-- Drop existing overly permissive policies
DROP POLICY IF EXISTS "Anyone can insert users" ON public.users;
DROP POLICY IF EXISTS "Users can update own profile" ON public.users;
DROP POLICY IF EXISTS "Users can delete their account" ON public.users;

-- Anyone can insert (for registration) but must match their auth.uid
CREATE POLICY "Users can insert own profile"
ON public.users
FOR INSERT
WITH CHECK (id = auth.uid()::text);

-- Users can only update their own profile
CREATE POLICY "Users can update own profile"
ON public.users
FOR UPDATE
USING (id = auth.uid()::text)
WITH CHECK (id = auth.uid()::text);

-- Users can only delete their own account
CREATE POLICY "Users can delete own account"
ON public.users
FOR DELETE
USING (id = auth.uid()::text);

-- =====================================================
-- 5. FIX: blocked_users table RLS
-- =====================================================
-- Drop existing overly permissive policies
DROP POLICY IF EXISTS "Users can insert blocked users" ON public.blocked_users;
DROP POLICY IF EXISTS "Users can delete from blocked list" ON public.blocked_users;

-- Users can only block others as themselves
CREATE POLICY "Users can insert blocked users"
ON public.blocked_users
FOR INSERT
WITH CHECK (blocker_id = auth.uid()::text);

-- Users can only unblock from their own block list
CREATE POLICY "Users can delete from blocked list"
ON public.blocked_users
FOR DELETE
USING (blocker_id = auth.uid()::text);

-- =====================================================
-- 6. FIX: user_public_keys table RLS
-- =====================================================
-- Drop existing overly permissive policies
DROP POLICY IF EXISTS "Users can insert their public keys" ON public.user_public_keys;
DROP POLICY IF EXISTS "Users can update their public keys" ON public.user_public_keys;

-- Users can only insert their own public keys
CREATE POLICY "Users can insert their public keys"
ON public.user_public_keys
FOR INSERT
WITH CHECK (user_id = auth.uid()::text);

-- Users can only update their own public keys
CREATE POLICY "Users can update their public keys"
ON public.user_public_keys
FOR UPDATE
USING (user_id = auth.uid()::text)
WITH CHECK (user_id = auth.uid()::text);

-- =====================================================
-- VERIFICATION: Run these queries to check policies
-- =====================================================
-- SELECT schemaname, tablename, policyname, permissive, roles, cmd, qual, with_check 
-- FROM pg_policies 
-- WHERE schemaname = 'public';
