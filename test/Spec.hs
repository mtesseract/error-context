{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Error.Context
import           Control.Monad.Reader

import           Control.Exception     (ArithException (..))
import           Control.Monad.Catch   (try)

-- | Some application environment.
data Env = Env { _errorContext :: ErrorContext }

-- | Make it error context aware.
instance HasErrorContext Env where
  errorContextLayerPush layer env =
    let errCtx = _errorContext env
    in env { _errorContext = errorContextPush layer errCtx }
  errorContextGet env =
    _errorContext env

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
