-- This table will store the one-time passcodes for email verification.
CREATE TABLE email_verification_otps (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    otp_hash TEXT NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- We need to add a column to the users table to track verification status.
ALTER TABLE users ADD COLUMN is_email_verified BOOLEAN DEFAULT FALSE;

ALTER TABLE users 
    ADD COLUMN IF NOT EXISTS failed_login_attempts INTEGER DEFAULT 0,
    ADD COLUMN IF NOT EXISTS locked_until TIMESTAMP WITH TIME ZONE;

CREATE TABLE IF NOT EXISTS refresh_tokens (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        token_hash VARCHAR(255) NOT NULL,
        expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
        is_revoked BOOLEAN DEFAULT FALSE,
        UNIQUE(token_hash)
      );

ALTER TABLE users ALTER COLUMN id SET DEFAULT gen_random_uuid();
alter table subscription_plans rename column id to subscription_plan_id;
