#!/bin/bash

set -e

echo "Sourcing environment variables from export-test-env.sh..."
source ./export-test-env.sh

cleanup() {
    echo "Stopping the test database..."
    make stopTestDB
}

trap cleanup EXIT

echo "Starting the test database via Docker..."
make startTestDB

echo "Waiting for 5 seconds for the database to initialize..."
sleep 5

echo "Running the test suite..."
stack test

echo "Test run finished."
