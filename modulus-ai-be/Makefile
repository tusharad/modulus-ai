# Makefile for modulus-ai Chatbot Application

# --- Configuration Variables ---
# These can be overridden by environment variables or command-line arguments.
# e.g., make up ENV_FILE=.env.production

# Default environment file
ENV_FILE ?= .env.local

# Docker Compose project name (useful for isolating environments)
# Defaults to the current directory's base name
PROJECT_NAME ?= $(notdir $(CURDIR))

# Stack build tool command
STACK_CMD ?= stack

# --- Phony Targets ---
# Declare targets that don't correspond to files, preventing conflicts with files named 'up', 'down', etc.
.PHONY: help up down logs status run build clean

# --- Default Target ---
# Running 'make' without arguments will show help.
.DEFAULT_GOAL := help

# --- Help Target ---
# Provides self-documenting help. Comments next to targets explain their purpose.
help: ## Display this help message
	@echo "Usage: make [target] ..."
	@echo ""
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

# --- Docker Compose Targets ---

# Bring up services defined in docker-compose.yml
up: ## Start Docker Compose services in detached mode
		@echo "Starting services using environment file: $(ENV_FILE) and project name: $(PROJECT_NAME)"
		@docker compose --env-file "$(ENV_FILE)" up -d
		@echo "Services started."

# Bring down services
down: ## Stop and remove Docker Compose services
	@echo "Stopping and removing services for project: $(PROJECT_NAME)"
	@docker compose --env-file "$(ENV_FILE)" down
	@echo "Services stopped and removed."

# View logs from services
logs: ## Follow logs from Docker Compose services
	@echo "Tailing logs for project: $(PROJECT_NAME)"
	@docker compose --env-file "$(ENV_FILE)" logs -f

# Check status of services
status: ## Show status of Docker Compose services
	@echo "Showing status for project: $(PROJECT_NAME)"
	@docker compose --env-file "$(ENV_FILE)" ps

# --- Application Targets ---

# Run the application using Stack
run: ## Run the application using 'stack run'
	@echo "Running the application..."
	@$(STACK_CMD) run

# Build the application using Stack
build: ## Build the application using 'stack build'
	@echo "Building the application..."
	@$(STACK_CMD) build

# Run tests for the application using stack
tests: ## Run tests for the application using `stack test`
	@echo "Running tests..."
	@$(STACK_CMD) test

# --- Cleanup Target ---

# Clean build artifacts (Stack specific)
clean: ## Remove Stack build artifacts
	@echo "Cleaning Stack build artifacts..."
	@$(STACK_CMD) clean
	@echo "Build artifacts cleaned."

startTestDB:
	docker run \
		--name modulus_test_pg_db \
		-e POSTGRES_PASSWORD=1234 \
		-e POSTGRES_USER=tushar \
		-e POSTGRES_DB=modulus_test_db \
		-p 5432:5432 \
		--rm -d \
		pgvector/pgvector:pg17-trixie

stopTestDB:
	docker stop modulus_test_pg_db
