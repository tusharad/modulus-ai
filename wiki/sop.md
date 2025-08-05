### **Project Modulus: Engineering Handbook & Standard Operating Procedures (SOP)**

**To:** The Engineering Team
**From:** Software Architect
**Date:** July 26, 2025
**Version:** 1.0
**Status:** Adopted

-----

### **1. Overview**

This document outlines the standard procedures, project structure, and technical guidelines for Project Medea. Adherence to these standards is mandatory to ensure code quality, maintainability, and a smooth development workflow. Our goal is to create a robust, scalable SaaS platform, and that begins with a disciplined approach to our engineering practices.

-----

### **2. Project Directory Structure**

We will use a single `cabal` project structure. This keeps build configurations centralized and simplifies dependency management. The layout is designed to scale as we add more internal libraries and executables.

```
medea/
├── .github/
│   └── workflows/
│       └── ci.yml             # GitHub Actions CI/CD pipeline
├── .vscode/
│   └── settings.json          # Recommended VSCode settings for HLS
├── app/
│   ├── api-server/
│   │   └── Main.hs            # Main entry for the primary API server executable
│   └── worker/
│       └── Main.hs            # Main entry for the background worker executable
├── config/
│   ├── development.cfg        # Configuration for local development
│   └── production.cfg         # Configuration for production
├── db/
│   └── migrations/
│       └── 0001_initial_schema.sql # SQL migration files
├── src/
│   ├── Medea/
│   │   ├── Api/               # Servant API types and handlers
│   │   ├── Core/              # Core domain logic and types
│   │   ├── Db/                # Database interaction logic (Esqueleto)
│   │   ├── Worker/            # Logic for background jobs (message consumption)
│   │   └── Config.hs          # Configuration loading module
│   └── Lib.hs                 # Main library module, exports all functionality
├── static/                    # Static assets (JS, CSS) - for now
├── test/
│   ├── Spec.hs                # Main test suite entry point
│   ├── Medea/
│   │   ├── ApiSpec.hs         # Tests for the API module
│   │   └── CoreSpec.hs        # Tests for the Core module
├── .dockerignore
├── .gitignore
├── .hlint.yaml                # HLint configuration
├── .ormolu                  # Ormolu configuration
├── cabal.project              # Cabal project file
├── docker-compose.yml         # Local development services
├── Dockerfile                 # Multi-stage Dockerfile for production builds
├── LICENSE
├── Makefile                   # Helper scripts for common tasks
├── medea.cabal                # Cabal package definition
└── README.md
```

-----

### **3. Local Environment Setup**

A reproducible local environment is critical. We will use a combination of local tooling and Docker for services.

#### **3.1. Required Software**

  * **GHC:** `9.6.5` or newer.
  * **Cabal:** `3.10` or newer.
  * **Haskell Language Server (HLS):** The latest version compatible with our GHC version.
  * **Docker and Docker Compose:** For running backing services.
  * **[Ormolu](https://github.com/tweag/ormolu):** Our chosen code formatter.
  * **[HLint](https://github.com/ndmitchell/hlint):** Our chosen linter.

#### **3.2. Initial Setup Steps**

1.  Clone the repository.
2.  Install GHC, Cabal, and HLS using `ghcup`.
3.  Install Ormolu: `cabal install ormolu`.
4.  Run `make services-up` to start PostgreSQL, RabbitMQ, and Redis via Docker Compose.
5.  Run `make build` to build the project and ensure all dependencies are fetched.
6.  Configure your editor to use HLS and Ormolu for formatting on save. A sample `.vscode/settings.json` is provided.

#### **3.3. `docker-compose.yml`**

This file defines the services needed for local development. It ensures every developer works with the same database and message bus versions.

```yaml
version: '3.8'
services:
  postgres:
    image: postgres:15
    container_name: medea-postgres
    environment:
      POSTGRES_USER: medea_user
      POSTGRES_PASSWORD: medea_password
      POSTGRES_DB: medea_dev
    ports:
      - "5432:5432"
    volumes:
      - medea_postgres_data:/var/lib/postgresql/data

  rabbitmq:
    image: rabbitmq:3-management
    container_name: medea-rabbitmq
    ports:
      - "5672:5672"  # AMQP port
      - "15672:15672" # Management UI

  redis:
    image: redis:7
    container_name: medea-redis
    ports:
      - "6379:6379"

volumes:
  medea_postgres_data:
```

-----

### **4. Coding Standards & Practices**

Consistency is key. All code submitted must adhere to these standards.

  * **Formatting:** All Haskell code **must** be formatted with **Ormolu**. This is non-negotiable and will be enforced by the CI pipeline.
  * **Linting:** All code **must** pass **HLint** checks with the configuration in `.hlint.yaml`.
  * **Warnings:** We will compile with `-Wall -Werror`. This means no warnings will be tolerated in the `main` branch.
  * **Imports:** Imports should be grouped and organized as follows:
    1.  External libraries (e.g., `Control.Monad.IO.Class`).
    2.  Project modules (e.g., `Medea.Core`).
    3.  Qualified imports.
  * **Error Handling:**
      * **No Partial Functions:** Avoid partial functions like `head`, `fromJust`, etc.
      * Use `ExceptT` for application-level errors that can be handled (e.g., validation failures).
      * Use standard exceptions for unexpected system-level errors.
  * **Testing:**
      * New features or business logic **must** be accompanied by **Hspec** tests.
      * Property-based tests using **QuickCheck** are strongly encouraged for functions operating on data structures.
      * All database queries must be tested to ensure correctness.

-----

### **5. Project SOP (Standard Operating Procedures)**

#### **5.1. Git Workflow**

We will use a simplified **GitHub Flow**:

1.  Create a new branch from `main` for each new feature or bugfix (e.g., `feature/add-user-auth` or `bugfix/fix-chat-history`).
2.  Commit your changes to this branch.
3.  Push your branch to the remote repository.
4.  Open a **Pull Request (PR)** targeting the `main` branch.
5.  The PR must be reviewed and approved by at least **one other engineer**.
6.  Once approved and CI passes, the PR can be merged into `main`.

#### **5.2. Code Reviews**

  * **PR Description:** The PR description must clearly explain the "what" and "why" of the changes. Link to the relevant issue in our tracker.
  * **Reviewer's Responsibility:** Review for correctness, clarity, performance, and adherence to standards. Provide constructive feedback.
  * **Author's Responsibility:** Respond to feedback and make necessary changes. Do not merge your own PRs.

-----

### **6. Infrastructure & CI/CD**

#### **6.1. CI Pipeline (GitHub Actions)**

The `.github/workflows/ci.yml` file will define our continuous integration pipeline. On every push to a PR, it will perform the following steps:

1.  **Lint & Format Check:** Verify that the code is formatted with Ormolu and passes HLint.
2.  **Build:** Compile the project with `cabal build --enable-tests`.
3.  **Test:** Run the test suite with `cabal test`.
4.  **Build Docker Image:** If the push is to the `main` branch, build a production Docker image and push it to our container registry (e.g., AWS ECR or Docker Hub).

#### **6.2. Helper Utilities (`Makefile`)**

A `Makefile` is provided to simplify common tasks.

```makefile
# Start all docker-compose services
services-up:
	docker-compose up -d

# Stop all docker-compose services
services-down:
	docker-compose down

# Build the project
build:
	cabal build all

# Run the test suite
test:
	cabal test all

# Format the code
format:
	ormolu -i $(shell find . -name "*.hs")

# Lint the code
lint:
	hlint .

# A full check to run before committing
pre-commit: format lint test
```
