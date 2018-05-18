# v0.3.0.0

New typeclass `MonadCatchRaw`.
Breaking changes in the API, removed some obsolete tests.
Improved throwing/catching logic in order to support rethrowing of exceptions.
Adjusted tests for new API.

# v0.2.1.0

Added instance `(MonadUnliftIO m, MonadCatch m) => MonadUnliftIO (ErrorContextT m)`.
