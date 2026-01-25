-- PostgreSQL schema for production deployment.
-- This mirrors the SQLite schema but uses Postgres-specific features.
-- Run this migration when moving from SQLite to Postgres.

-- Messages table
CREATE TABLE IF NOT EXISTS messages (
    id TEXT PRIMARY KEY,
    chat_id TEXT NOT NULL,
    sender_id TEXT NOT NULL,
    content TEXT NOT NULL,
    timestamp TIMESTAMPTZ NOT NULL,
    status TEXT NOT NULL DEFAULT 'sent',
    type TEXT NOT NULL DEFAULT 'text',
    expires_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for listing messages by chat (primary query path)
CREATE INDEX IF NOT EXISTS idx_messages_chat_id_timestamp 
    ON messages(chat_id, timestamp);

-- Index for filtering expired messages
CREATE INDEX IF NOT EXISTS idx_messages_chat_id_expires_at 
    ON messages(chat_id, expires_at) 
    WHERE expires_at IS NOT NULL;

-- Partial index for active (non-expired) messages
CREATE INDEX IF NOT EXISTS idx_messages_active
    ON messages(chat_id, timestamp)
    WHERE expires_at IS NULL OR expires_at > NOW();

-- Idempotency keys table
CREATE TABLE IF NOT EXISTS idempotency_keys (
    key TEXT PRIMARY KEY,
    message_id TEXT NOT NULL REFERENCES messages(id),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for TTL-based cleanup
CREATE INDEX IF NOT EXISTS idx_idempotency_created_at 
    ON idempotency_keys(created_at);

-- Optional: Add partitioning for high-volume deployments
-- PARTITION BY RANGE (created_at);

-- Optional: Add row-level security for multi-tenant
-- ALTER TABLE messages ENABLE ROW LEVEL SECURITY;
-- CREATE POLICY tenant_isolation ON messages
--     USING (tenant_id = current_setting('app.tenant_id'));
