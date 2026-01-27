-- ===========================================
-- CALL SIGNALING TABLE FOR WEBRTC
-- ===========================================
-- Stores WebRTC signaling messages (offers, answers, ICE candidates)
-- Run this in your Supabase SQL Editor

CREATE TABLE IF NOT EXISTS call_signaling (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  caller_id TEXT NOT NULL,
  callee_id TEXT NOT NULL,
  type TEXT NOT NULL CHECK (type IN ('offer', 'answer', 'ice-candidate', 'end', 'reject', 'busy')),
  payload JSONB,
  call_type TEXT DEFAULT 'video' CHECK (call_type IN ('video', 'audio')),
  call_id TEXT NOT NULL,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_call_signaling_caller_id ON call_signaling(caller_id);
CREATE INDEX IF NOT EXISTS idx_call_signaling_callee_id ON call_signaling(callee_id);
CREATE INDEX IF NOT EXISTS idx_call_signaling_call_id ON call_signaling(call_id);
CREATE INDEX IF NOT EXISTS idx_call_signaling_created_at ON call_signaling(created_at DESC);

-- Enable Row Level Security
ALTER TABLE call_signaling ENABLE ROW LEVEL SECURITY;

-- Policies for call_signaling table
CREATE POLICY "Users can view their own signaling messages" ON call_signaling
  FOR SELECT USING (true);

CREATE POLICY "Users can insert signaling messages" ON call_signaling
  FOR INSERT WITH CHECK (true);

CREATE POLICY "Users can delete their signaling messages" ON call_signaling
  FOR DELETE USING (true);

-- Enable Realtime for call_signaling
ALTER PUBLICATION supabase_realtime ADD TABLE call_signaling;

-- Auto-cleanup old signaling messages (older than 5 minutes)
-- This function can be called periodically via Supabase Edge Functions or pg_cron
CREATE OR REPLACE FUNCTION cleanup_old_signaling()
RETURNS void AS $$
BEGIN
  DELETE FROM call_signaling WHERE created_at < NOW() - INTERVAL '5 minutes';
END;
$$ LANGUAGE plpgsql;
