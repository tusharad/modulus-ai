-- =============================================================================
-- SUBSCRIPTION SETUP
-- =============================================================================

-- First, add user_id column to user_subscriptions table (it's missing from original schema)
ALTER TABLE user_subscriptions ADD COLUMN user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE;

-- Create index for efficient lookup by user_id
CREATE INDEX idx_user_subscriptions_user_id ON user_subscriptions(user_id);

-- Ensure one subscription per user (business rule)
ALTER TABLE user_subscriptions ADD CONSTRAINT unique_user_subscription UNIQUE(user_id);
