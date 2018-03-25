# error-context

Provides API for enriching errors with contexts.

Example:

```
[...]
main :: IO ()
main = do
  flip runReaderT (Env errorContextRoot) $ do
    res :: Either (ErrorWithContext ArithException) () <- try test
    case res of
      Right _ -> liftIO $ putStrLn "OK"
      Left (ErrorWithContext _ errCtx) -> do
        liftIO $ putStrLn "ERROR, error context:"
        errorContextDump errCtx

test :: ReaderT Env IO ()
test = withErrorContext "test function" $ do
  execRequest

execRequest :: ReaderT Env IO ()
execRequest = withErrorContext "execRequest" $ do
  throwWithContext Overflow

```
