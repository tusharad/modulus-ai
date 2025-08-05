### **User Story MED-4.2: Define the Core Application Monad (`AppM`)**

*As a developer, I need a custom application monad stack (`AppM`) so that we have a unified, extensible, and ergonomic way to handle configuration, errors, logging, and other application-wide concerns across the entire backend.*

-----

#### **Acceptance Criteria**

1.  **Monad Stack Definition**:

      * A new module, `BE.AppM`, is created to define the core application monad and its environment.
      * The monad, `AppM`, must be a `newtype` wrapper around a monad transformer stack built on `IO`.
      * The stack must include transformers for:
          * **Configuration (`ReaderT`):** To provide read-only access to application-wide resources like the database connection pool and logger handle.
          * **Error Handling (`ExceptT`):** To provide structured, typed error handling for recoverable application failures.

2.  **Application Environment (`AppEnv`)**:

      * A record type, `AppEnv`, is defined to hold all shared, read-only application resources.
      * Initially, `AppEnv` will contain the database connection pool (`Orville.ConnectionPool`) and the `LoggerHandle` from the logging story.
      * This environment must be designed to be easily extensible with future resources (e.g., HTTP client managers, Stripe clients).

3.  **Custom Error Type (`AppError`)**:

      * A custom Algebraic Data Type (ADT), `AppError`, is defined to represent all *recoverable* application-level errors.
      * Initial error constructors must include `AuthenticationError`, `ValidationError Text`, and `ResourceNotFound Text`.
      * The design must allow for easy addition of new error types as the application grows. This is preferable to using `String` or `SomeException` for application logic errors.

4.  **Ergonomic Runner Function**:

      * A function `runAppM :: AppEnv -> AppM a -> IO (Either AppError a)` is created.
      * This function will be the single entry point for executing an `AppM` computation in the `IO` monad, handling the setup and teardown of the monad stack.

5.  **Derivations and Instances**:

      * The `AppM` newtype must derive standard instances for `Functor`, `Applicative`, and `Monad`.
      * It must also have instances for `MonadIO`, `MonadReader AppEnv`, and `MonadError AppError` to provide direct access to the capabilities of the underlying transformers without verbose lifting.
      * An instance for `MonadLogger` will be defined for `AppM` once the logging story is complete.

6.  **Integration and Demonstration**:

      * The `main` function is refactored to initialize the `AppEnv` and use `runAppM` to execute the main application logic.
      * At least one API handler (e.g., a test `/health` endpoint) is rewritten to operate within the `AppM` monad, demonstrating its use for database access (via the `AppEnv`) and error handling.

-----

#### **Technical Tasks & Implementation Plan**

1.  **Create the `BE.AppM` Module**:

      * Define the `AppEnv` record.
      * Define the `AppError` ADT.
      * Define the `AppM` newtype: `newtype AppM a = AppM { runAppM' :: ReaderT AppEnv (ExceptT AppError IO) a }`.

2.  **Implement Typeclass Instances**:

      * Use the `GeneralizedNewtypeDeriving` extension to automatically derive `Functor`, `Applicative`, `Monad`, `MonadIO`, `MonadReader AppEnv`, and `MonadError AppError`. This is the standard, low-boilerplate approach.

3.  **Write the Runner Function**:

      * Implement `runAppM` which will take the environment and the `AppM` computation and use `runExceptT` and `runReaderT` to execute it.

4.  **Create Helper Functions**:

      * Write helper functions within `BE.AppM` to make common tasks easier, for example:
        ```haskell
        -- Lifts an IO action that can throw exceptions into AppM, converting exceptions to a specific AppError.
        liftIOE :: (Exception e) => (e -> AppError) -> IO a -> AppM a

        -- A helper to easily access the DB pool
        getDbPool :: AppM Orville.ConnectionPool
        getDbPool = asks appEnvDbPool
        ```

5.  **Refactor `main`**:

      * The `main` function will be responsible for:
        1.  Initializing the `LoggerHandle`.
        2.  Creating the database connection pool.
        3.  Constructing the `AppEnv`.
        4.  Calling `runAppM` with the environment and the main server logic.
        5.  Handling the final `Either AppError a` result (e.g., logging the error and exiting gracefully if `Left`).

6.  **Write Tests**:

      * Create `test/BE/AppMSpec.hs`.
      * Write tests that construct a mock `AppEnv` and run simple `AppM` computations to verify that:
          * `MonadReader` functionality works (can read from the environment).
          * `MonadError` functionality works (`throwError` and `catchError` behave as expected).

-----

#### **Architectural Vision & Best Practices**

This design establishes a powerful and flexible foundation for our entire application.

  * **Scalability**: The `ReaderT AppEnv` pattern is the Haskell standard for dependency injection. It allows us to add new application-wide resources without changing function signatures across the codebase. We simply add a field to `AppEnv`.
  * **Robustness**: Using `ExceptT` with a custom `AppError` type makes our error handling explicit and type-safe. It forces developers to handle specific failure cases, preventing the kind of bugs that arise from untyped string exceptions.
  * **Maintainability**: By providing instances for `MonadReader`, `MonadError`, etc., developers can use familiar functions like `ask`, `throwError`, and `liftIO` directly, making the business logic clean and readable. The complexity of the monad stack is hidden behind the `AppM` abstraction.

Adopting this structure is a critical step towards building a truly production-grade Haskell application.
