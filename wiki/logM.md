### **User Story MED-4.5: Implement a Swappable, Structured Logging System**

*As a developer, I need a structured, level-based logging system with a swappable backend so that I can effectively monitor application behavior during development and have a clear path to integrate with production monitoring services like Datadog in the future.*

-----

#### **Acceptance Criteria**

1.  **Logging Abstraction**: A new typeclass, `MonadLogger`, is defined. It must provide logging functions for at least four standard severity levels: `logDebug`, `logInfo`, `logWarn`, and `logError`.
2.  **Structured Logging**: All log messages, regardless of the backend, must be structured as JSON objects. Each log entry must contain a minimum set of fields:
      * `timestamp`: The UTC time of the log event (ISO 8601 format).
      * `level`: The severity level (`"DEBUG"`, `"INFO"`, `"WARN"`, `"ERROR"`).
      * `message`: The primary log message as a string.
3.  **Context-Aware Logging**: The system must support adding contextual key-value pairs to log messages. For example, a web request handler should be able to add `{"request_id": "uuid", "user_id": "uuid"}` to all logs generated within its scope.
4.  **Initial `stdout` Backend**: The default, initial implementation of the `MonadLogger` will be a "Console Logger" that:
      * Serializes the structured log entry (including context) into a JSON string.
      * Prints the JSON string to `stdout`.
      * Respects a minimum log level set by an environment variable (e.g., `LOG_LEVEL=INFO`). If the level is set to `INFO`, `logDebug` calls should do nothing.
5.  **Application Integration**: The new logging capability is integrated into the application's core monad (e.g., `AppM`). This makes `logInfo "..."` and other logging functions available in all API handlers and business logic.
6.  **Demonstrable Usage**: At least two distinct parts of the existing application (e.g., the database connection module and a test endpoint) are updated to use the new logging system to demonstrate its functionality.
7.  **Testing**: The logging system is unit-tested. Tests must verify:
      * Logs are correctly formatted as JSON.
      * Contextual data is correctly included in the output.
      * The `LOG_LEVEL` environment variable is respected.

-----

#### **Technical Tasks & Implementation Plan**

1.  **Design the `MonadLogger` Typeclass**:

      * Create a new module, `BE.Log`.
      * Define the `LogLevel` ADT: `data LogLevel = Debug | Info | Warn | Error`.
      * Define the core typeclass:
        ```haskell
        class Monad m => MonadLogger m where
          logM :: LogLevel -> Text -> [(Text, Aeson.Value)] -> m ()

        -- Helper functions
        logDebug :: MonadLogger m => Text -> m ()
        logInfo  :: MonadLogger m => Text -> m ()
        logWarn  :: MonadLogger m => Text -> m ()
        logError :: MonadLogger m => Text -> m ()

        -- Function to add context
        withLogContext :: MonadLogger m => (Text, Aeson.Value) -> m a -> m a
        ```

2.  **Implement the Console Logger**:

      * Create a new module, `BE.Log.Backend.Console`.
      * Define a `LoggerHandle` data type that holds the minimum log level and any other configuration.
      * Create a function `newConsoleLogger :: IO LoggerHandle` that reads the `LOG_LEVEL` from the environment.
      * Write the function `runLoggerWith :: LoggerHandle -> AppM a -> IO a` which will provide the concrete implementation for the `MonadLogger` instance. The implementation will use `IO.putStrLn` and the `Aeson` library to format the log entry.

3.  **Integrate into the Application Monad**:

      * The application's main monad transformer stack (`AppM`) will be updated to include a `ReaderT LoggerHandle`.
      * An instance `MonadLogger AppM` will be created. The `logM` function will read the `LoggerHandle` from its environment and use it to process the log message.

4.  **Refactor Existing Code**:

      * Modify the `main` function to initialize the `LoggerHandle` and run the application with it.
      * Replace existing `putStrLn` or `print` statements in at least two key areas (e.g., `BE.DB.Internal.Config` on successful connection, and create a new `/test-log` endpoint) with calls to `logInfo`, `logError`, etc.

5.  **Write Tests**:

      * Create a new test suite in `test/BE/LogSpec.hs`.
      * Write HUnit test cases to capture the `stdout` of the logger and verify that the output is valid JSON and contains the correct fields and context.
      * Test that setting `LOG_LEVEL=WARN` correctly suppresses `INFO` and `DEBUG` messages.

This story provides a robust, production-ready logging foundation that meets the immediate need for visibility while ensuring we can easily swap to a more advanced backend like Datadog without rewriting our application code.
