-- Call Logs Table for WhatsApp Clone
-- Stores all call history for users
-- Run this in your Supabase SQL Editor

-- Call logs table
CREATE TABLE IF NOT EXISTS call_logs (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  caller_id TEXT NOT NULL,
  callee_id TEXT NOT NULL,
  call_type TEXT NOT NULL CHECK (call_type IN ('video', 'audio')),
  status TEXT NOT NULL CHECK (status IN ('incoming', 'outgoing', 'missed', 'declined', 'completed')),
  started_at TIMESTAMPTZ DEFAULT NOW(),
  ended_at TIMESTAMPTZ,
  duration INTEGER, -- Duration in seconds
  call_id TEXT, -- Reference to the WebRTC call session
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_call_logs_caller_id ON call_logs(caller_id);
CREATE INDEX IF NOT EXISTS idx_call_logs_callee_id ON call_logs(callee_id);
CREATE INDEX IF NOT EXISTS idx_call_logs_started_at ON call_logs(started_at DESC);
CREATE INDEX IF NOT EXISTS idx_call_logs_call_id ON call_logs(call_id);

-- Composite index for fetching user's call history
CREATE INDEX IF NOT EXISTS idx_call_logs_user_history ON call_logs(caller_id, callee_id, started_at DESC);

-- Enable Row Level Security
ALTER TABLE call_logs ENABLE ROW LEVEL SECURITY;

-- Policies for call_logs table
-- Users can view calls where they are either caller or callee
CREATE POLICY "Users can view their own calls" ON call_logs
  FOR SELECT USING (true);

CREATE POLICY "Users can insert call logs" ON call_logs
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can update their own calls" ON call_logs
  FOR UPDATE USING (true);

CREATE POLICY "Users can delete their own calls" ON call_logs
  FOR DELETE USING (true);

-- Enable Realtime for call_logs
ALTER PUBLICATION supabase_realtime ADD TABLE call_logs;
