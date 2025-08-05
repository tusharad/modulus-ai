### **Project Medea: Q3 Sprint 1 Plan - "The Great Migration"**

**To**: The Engineering Team
**From**: Gemini, Software Architect
**Date**: July 26, 2025
**Sprint Duration**: 2 Weeks (Monday, July 28 - Friday, August 8, 2025)

---

### **1. Sprint Goal**

**By the end of this sprint, we will have completely decommissioned SQLite and fully migrated our application's data layer to a multi-tenant PostgreSQL database. All code will be updated to use the new database, all tests will pass, and the local development environment will be running against a PostgreSQL container.**

This sprint is foundational. Its successful completion is critical for all future work this quarter. We will prioritize quality and thoroughness over speed.

---

### **2. Sprint Ceremonies**

* **Daily Standup**: 10:00 AM IST, Daily (15 minutes)
* **Sprint Planning**: Monday, July 28, 10:00 AM IST (This meeting)
* **Backlog Refinement**: Friday, August 1, 2:00 PM IST (1 hour)
* **Sprint Review & Retrospective**: Friday, August 8, 3:00 PM IST (1.5 hours)

---

### **3. Sprint Backlog & Stories**

The work is broken down into one primary Epic with several user stories.

**EPIC 1: Foundational Database Migration**
*As a developer, I want the application to use PostgreSQL instead of SQLite so that we have a scalable, production-ready, and multi-tenant data persistence layer.*

---

#### **User Stories for Sprint 1**

**Story MED-1: Environment Setup**
*As a developer, I need to set up my local environment with PostgreSQL so that I can develop and test against the new database.*

* **Acceptance Criteria**:
    1.  The `docker-compose.yml` file includes a PostgreSQL 15 service.
    2.  Running `make services-up` successfully starts the PostgreSQL container alongside other services.
    3.  The `medea.cabal` file is updated to remove the `sqlite-simple` dependency and add `postgresql-simple`, `esqueleto`, and `postgresql-migration`.
    4.  A `cabal build` completes successfully with the new dependencies.
* **Technical Tasks**:
    * Create the `docker-compose.yml` file as defined in the SOP.
    * Modify `medea.cabal` to update library dependencies.
    * Ensure all engineers can run `docker-compose up -d` and `cabal build` without errors.

---

**Story MED-2: Database Schema & Migration**
*As a developer, I need a multi-tenant database schema defined and a migration script created so that we have a version-controlled and repeatable way to set up the database structure.*

* **Acceptance Criteria**:
    1.  A SQL file exists in `db/migrations/0001_initial_schema.sql`.
    2.  This script, when executed, creates `conversations` and `chat_history` tables.
    3.  Every table contains an `owner_id` (or similar, e.g., `user_id`) column to enforce data isolation. Foreign key constraints are correctly defined.
    4.  Primary keys, indexes (e.g., on foreign keys and timestamps), and constraints are defined as per the old schema, plus the new multi-tenancy column.
* **Technical Tasks**:
    * Analyze the `createTables` function in the existing `src/App/DB.hs` to understand the current schema.
    * Design the new PostgreSQL schema, paying close attention to data types and adding the `owner_id` column.
    * Write the `0001_initial_schema.sql` script.
    * Manually apply the script to the local PostgreSQL instance to verify its correctness.

---

**Story MED-3: Refactor Database Connection & Core Logic**
*As a developer, I need to refactor the core database functions to use PostgreSQL-specific libraries so that our application can communicate with the new database.*

* **Acceptance Criteria**:
    1.  The `src/App/DB.hs` module is completely rewritten.
    2.  The `withDatabase` function now connects to PostgreSQL using connection details from a configuration file.
    3.  All functions (e.g., `createConversation`, `getConversationMessages`, `addMessage`) are re-implemented using `postgresql-simple` and `esqueleto`.
    4.  All function type signatures are updated to include the `owner_id` where necessary for querying, ensuring multi-tenancy.
    5.  The `CRole` custom field instances for `ToField` and `FromField` are verified to be compatible with PostgreSQL ENUM types or are adapted appropriately.
* **Technical Tasks**:
    * **This is the largest task.** The team will swarm this.
    * Create a new `Medea.Db.Connection` module to handle PostgreSQL connection pooling.
    * Rewrite each function in `src/App/DB.hs`, one by one.
    * Pay special attention to parameter substitution (`?` in `sqlite-simple` vs. `?` in `postgresql-simple`) and error handling.
    * Use `esqueleto` for type-safe queries where possible, especially for `SELECT` statements.

---

**Story MED-4: Update and Pass All Tests**
*As a developer, I need to update our test suite to run against the new PostgreSQL database so that I can be confident that the migration has not introduced regressions.*

* **Acceptance Criteria**:
    1.  The test suite successfully connects to a separate test database in PostgreSQL.
    2.  All existing tests are refactored to work with the new database functions.
    3.  The test suite cleans up and re-migrates the test database before each run to ensure test isolation.
    4.  `make test` or `cabal test` runs all tests, and they all pass.
* **Technical Tasks**:
    * Create a test helper function that sets up and tears down a test database.
    * Modify test cases to pass the required `owner_id` for multi-tenancy checks.
    * Adapt tests to any changes in the data types returned by the new database functions.

---

### **4. Definition of Done**

A story is considered "Done" only when:
1.  All code has been formatted with Ormolu and passes HLint.
2.  All code has been reviewed and approved by at least one other engineer in a PR.
3.  All acceptance criteria for the story are met.
4.  All related unit and integration tests pass in the CI pipeline.
5.  The changes are successfully merged into the `main` branch.

The sprint is "Done" when all stories are "Done" and the application runs flawlessly against PostgreSQL in the local development environment.
