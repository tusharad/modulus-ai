### **Project Medea: Low-Level Authentication Design**

#### **1. Core Principles**

  * **Security First**: We will never store passwords in plaintext. All passwords will be hashed using a strong, modern algorithm like **bcrypt**.
  * **Stateless Authentication with JWT**: We will use JSON Web Tokens (JWT) for authenticating API requests. This is efficient and scales well in a distributed system.
  * **Defense in Depth**: We will use multiple layers of security, including email verification, secure password hashing, and short-lived access tokens.
  * **Clear Separation of Concerns**: The authentication logic will be isolated in its own set of modules, separate from the core application logic.

-----

#### **2. Database Schema Additions**

In addition to the previously defined `users` table, we need a table to store email verification OTPs.

```sql
-- This table will store the one-time passcodes for email verification.
CREATE TABLE email_verification_otps (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    otp_hash TEXT NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- We need to add a column to the users table to track verification status.
ALTER TABLE users ADD COLUMN is_email_verified BOOLEAN NOT NULL DEFAULT FALSE;
```

-----

#### **3. User Registration Flow**

This flow ensures that a user can only log in after verifying that they own the email address they signed up with.

**Sequence Diagram: Registration**

**Step-by-Step Breakdown**:

1.  **User Submits Registration Form**: The user provides their `email` and `password` to the `/api/register` endpoint.
2.  **API Server Receives Request**:
      * **Validate Input**: The server validates that the email is in a valid format and the password meets complexity requirements (e.g., minimum length).
      * **Check for Existing User**: The server queries the `users` table to see if a user with that email already exists. If so, it returns an error.
3.  **Create User Record**:
      * **Hash Password**: The server hashes the user's password using `bcrypt`.
      * **Create User**: A new record is inserted into the `users` table with `is_email_verified` set to `FALSE`.
4.  **Generate and Send OTP**:
      * **Generate OTP**: A secure, random 6-digit OTP is generated.
      * **Hash OTP**: The OTP is hashed using `bcrypt` and stored in the `email_verification_otps` table along with the `user_id` and an expiration time (e.g., 10 minutes).
      * **Send Email**: The server calls the Mailgun API to send the plaintext OTP to the user's email address.
5.  **User Submits OTP**: The user receives the OTP and submits it to the `/api/verify-email` endpoint along with their email address.
6.  **API Server Verifies OTP**:
      * **Find User and OTP**: The server finds the user by email and retrieves the latest OTP hash from the `email_verification_otps` table.
      * **Check Expiration**: It verifies that the OTP has not expired.
      * **Verify OTP**: It hashes the submitted OTP and compares it with the stored hash.
7.  **Activate User Account**:
      * If the OTP is valid, the server updates the `users` table, setting `is_email_verified` to `TRUE`.
      * It then deletes the used OTP from the `email_verification_otps` table.
      * The server returns a success message to the user, who can now log in.

-----

#### **4. User Login Flow**

This flow authenticates the user and provides them with a set of tokens to access the application.

**Sequence Diagram: Login**

**Step-by-Step Breakdown**:

1.  **User Submits Login Form**: The user provides their `email` and `password` to the `/api/login` endpoint.
2.  **API Server Receives Request**:
      * **Find User**: The server queries the `users` table to find a user with the given email. If no user is found, it returns an "Invalid credentials" error.
      * **Check Verification Status**: The server checks if `is_email_verified` is `TRUE`. If not, it returns an error prompting the user to verify their email.
3.  **Verify Password**:
      * The server uses the `bcrypt` library to compare the submitted password with the `hashed_password` stored in the database.
      * If the password does not match, it returns an "Invalid credentials" error.
4.  **Generate JWTs**:
      * If the password is correct, the server generates two JWTs:
          * **Access Token**: A short-lived token (e.g., 15 minutes) that contains the user's ID and permissions. This token will be sent with every authenticated API request.
          * **Refresh Token**: A long-lived token (e.g., 30 days) that is used to obtain a new access token when the old one expires.
5.  **Return Tokens**: The server returns the `access_token` and `refresh_token` to the user in the response body. The client should store these securely (e.g., the access token in memory and the refresh token in an `HttpOnly` cookie).

-----

#### **5. JWT Strategy**

  * **Access Token Payload**:
    ```json
    {
      "sub": "user_id_uuid",
      "org": "organization_id_uuid",
      "role": "admin",
      "exp": 1722000000
    }
    ```
  * **Refresh Token**: The refresh token will be an opaque string stored in a `sessions` table in the database, linked to a user ID. This allows us to invalidate sessions and implement features like "log out from all devices."
  * **Token Refresh Flow**: When the access token expires, the client sends the refresh token to an `/api/refresh-token` endpoint. The server validates the refresh token against the `sessions` table and, if valid, issues a new access token.

This detailed design provides a clear path for implementing a secure and robust authentication system. The team should focus on implementing each step carefully, with thorough testing at every stage.
