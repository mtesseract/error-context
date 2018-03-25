{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Control.Error.Context
  ( ErrorContext
  , ErrorContextLayer
  , ErrorWithContext(..)
  , HasErrorContext(..)
  , errorContextRoot
  , withErrorContext
  , errorContextDump
  , throwWithContext
  , errorContextAdd
  , errorForgetContext
  , errorContextPush
  ) where

import           Control.Monad.Catch  (Exception, MonadThrow, throwM)
import           Control.Monad.Reader
import           Data.Monoid
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Typeable

errorContextLayerUnpack :: ErrorContextLayer -> Text
errorContextLayerUnpack (ErrorContextLayer t) = t

data ErrorContextLayer =
  ErrorContextLayer Text deriving (Show, Eq)
data ErrorContext = ErrorContext [ErrorContextLayer] deriving (Show, Eq)

instance IsString ErrorContextLayer where
  fromString = ErrorContextLayer . Text.pack

class HasErrorContext env where
  errorContextLayerPush :: ErrorContextLayer -> env -> env
  errorContextGet       :: env -> ErrorContext

errorContextPush :: ErrorContextLayer -> ErrorContext -> ErrorContext
errorContextPush layer (ErrorContext layers) =
  ErrorContext (layer : layers)

data ErrorWithContext e =
  ErrorWithContext e ErrorContext
  deriving (Show, Typeable)

instance Exception e => Exception (ErrorWithContext e)

errorContextRoot :: ErrorContext
errorContextRoot = ErrorContext []

withErrorContext
  :: (HasErrorContext r, MonadReader r m)
  => ErrorContextLayer
  -> m a
  -> m a
withErrorContext layer =
  local (errorContextLayerPush layer)

errorContextDump :: MonadIO m => ErrorContext -> m ()
errorContextDump (ErrorContext (layer : layers)) = do
  liftIO . putStrLn . Text.unpack . errorContextLayerUnpack $ layer
  forM_ layers $ \ (ErrorContextLayer layer) -> do
    let layerS = Text.unpack layer
    liftIO . putStrLn $ "Caused by: " <> layerS

-- | Context aware replacement for 'throwM'.
throwWithContext
  :: (Exception e, MonadReader r m, HasErrorContext r, MonadThrow m)
  => e
  -> m a
throwWithContext exn = do
  errCtx <- asks errorContextGet
  throwM $ ErrorWithContext exn errCtx

-- | Enrich an error with context.
errorContextAdd
  :: (MonadReader r m, HasErrorContext r)
  => e
  -> m (ErrorWithContext e)
errorContextAdd e = do
  errCtx <- asks errorContextGet
  pure $ ErrorWithContext e errCtx

-- | Forgets a context from an enriched error.
errorForgetContext :: ErrorWithContext e -> e
errorForgetContext (ErrorWithContext e _errCtx) = e
