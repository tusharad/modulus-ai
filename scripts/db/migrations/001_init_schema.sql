-- =============================================================================
-- ENUM TYPES
-- =============================================================================
-- Using ENUMs for fixed sets of values provides type safety and is more
-- efficient than using strings.

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TYPE user_role AS ENUM ('admin', 'member');
CREATE TYPE message_role AS ENUM ('user', 'assistant', 'system', 'tool');
CREATE TYPE subscription_status AS ENUM ('active', 'past_due', 'canceled', 'trialing');

-- =============================================================================
-- ORGANIZATIONS & TENANCY
-- =============================================================================
-- The core of our multi-tenancy model. Every major resource will be tied to an
-- organization.

CREATE TABLE organizations (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for efficient lookup by creation date.
CREATE INDEX idx_organizations_created_at ON organizations(created_at);

-- =============================================================================
-- USERS & AUTHENTICATION
-- =============================================================================

CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email TEXT NOT NULL UNIQUE,
    hashed_password TEXT NOT NULL,
    last_login_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for efficient lookup by email.
CREATE INDEX idx_users_email ON users(email);

-- Junction table to link users to organizations with a specific role.
CREATE TABLE organization_members (
    organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    role user_role NOT NULL DEFAULT 'member',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (organization_id, user_id)
);

CREATE INDEX idx_organization_members_user_id ON organization_members(user_id);

-- =============================================================================
-- CORE CHAT FUNCTIONALITY
-- =============================================================================

CREATE TABLE conversations (
    id BIGSERIAL PRIMARY KEY,
    public_id UUID NOT NULL UNIQUE DEFAULT gen_random_uuid(), -- For external use
    organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
    user_id UUID REFERENCES users(id) ON DELETE SET NULL,
                            --User who started the conversation
    title TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_conversations_organization_id ON conversations(organization_id);
CREATE INDEX idx_conversations_user_id ON conversations(user_id);

-- The chat_messages table is now a standard table with a foreign key.
CREATE TABLE chat_messages (
    id BIGSERIAL PRIMARY KEY,
    public_id UUID NOT NULL UNIQUE DEFAULT gen_random_uuid(),
    conversation_id BIGINT NOT NULL REFERENCES conversations(id) ON DELETE CASCADE,
    organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE, 
        -- Denormalized for efficient RLS
    role message_role NOT NULL,
    content TEXT NOT NULL,
    model_used TEXT, -- e.g., 'gpt-4', 'claude-2'
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_chat_messages_conversation_id ON chat_messages(conversation_id);
CREATE INDEX idx_chat_messages_organization_id ON chat_messages(organization_id);

CREATE TABLE message_attachments (
    id BIGSERIAL PRIMARY KEY,
    message_id BIGINT NOT NULL,
    organization_id UUID NOT NULL,
    file_name TEXT NOT NULL,
    file_type TEXT NOT NULL,
    file_size_bytes BIGINT NOT NULL,
    storage_path TEXT NOT NULL, -- e.g., an S3 key
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_message_attachments_message_id ON message_attachments(message_id);

-- =============================================================================
-- SUBSCRIPTIONS & BILLING
-- =============================================================================

CREATE TABLE subscription_plans (
    id TEXT PRIMARY KEY, -- e.g., 'free', 'pro', 'enterprise'
    name TEXT NOT NULL,
    price_cents INTEGER NOT NULL DEFAULT 0,
    features JSONB -- e.g., {"max_users": 5, "max_bots": 1}
);

CREATE TABLE user_subscriptions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    organization_id UUID NOT NULL UNIQUE REFERENCES organizations(id) ON DELETE CASCADE,
    plan_id TEXT NOT NULL REFERENCES subscription_plans(id),
    stripe_subscription_id TEXT UNIQUE,
    status subscription_status NOT NULL,
    current_period_ends_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- =============================================================================
-- AUDITING
-- =============================================================================
CREATE TABLE audit_log (
    id BIGSERIAL PRIMARY KEY,
    organization_id UUID,
    user_id UUID,
    action TEXT NOT NULL, -- e.g., 'user.login', 'conversation.delete'
    details JSONB,
    ip_address INET,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_audit_log_organization_id ON audit_log(organization_id);
CREATE INDEX idx_audit_log_user_id ON audit_log(user_id);
CREATE INDEX idx_audit_log_action ON audit_log(action);

-- =============================================================================
-- ROW-LEVEL SECURITY (RLS) POLICIES
-- =============================================================================
-- RLS is a powerful feature for enforcing multi-tenancy at the database level.
-- These are example comments; the actual policies would be implemented in code.

-- ALTER TABLE conversations ENABLE ROW LEVEL SECURITY;
-- CREATE POLICY conversations_isolation_policy ON conversations
--     FOR ALL
--     USING (organization_id = current_setting('app.current_organization_id')::UUID);

-- ALTER TABLE chat_messages ENABLE ROW LEVEL SECURITY;
-- CREATE POLICY chat_messages_isolation_policy ON chat_messages
--     FOR ALL
--     USING (organization_id = current_setting('app.current_organization_id')::UUID);
