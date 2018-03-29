{-|
Module      : Control.Error.Context
Description : API for enriching errors with contexts
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX

Provides an API for enriching errors with contexts.
-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Error.Context
  ( ErrorContext(..)
  , ErrorContextT
  , MonadErrorContext(..)
  , ErrorWithContext(..)
  , runErrorContextT
  , errorContextualize
  , errorContextForget
  , errorContextualizeIO
  , errorWithContextDump
  , catchWithContext
  )
  where

import           Control.Exception.Safe       (SomeException (..), catchAny)
import           Control.Monad.Catch          (Exception, MonadCatch (..),
                                               MonadThrow, throwM)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

-- | Monad type class providing contextualized errors.
class (Monad m, MonadThrow m) => MonadErrorContext m where
  errorContextCollect :: m ErrorContext
  withErrorContext :: Text -> m a -> m a

-- | Data type implementing 'MonadErrorContext'.
newtype ErrorContextT m a =
  ErrorContextT { _runErrorContextT :: ReaderT ErrorContext m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadTrans
                           , MonadState s
                           , MonadWriter w )

-- | Unwrap an 'ErrorContextT'. Exceptions of type @e@ thrown in the
-- provided action via 'throwM' will cause an @'ErrorWithContext' e@
-- exception to be propagated upwards.
runErrorContextT :: ErrorContextT m a -> m a
runErrorContextT ctx =
  runReaderT (_runErrorContextT ctx) (ErrorContext [])

instance (MonadCatch m, MonadIO m) => MonadIO (ErrorContextT m) where
  liftIO m = do
    ctx <- errorContextCollect
    lift . liftIO $ catchAny m $ \ (SomeException exn) -> throwM (ErrorWithContext exn ctx)

-- | Implement 'MonadErrorContext' for 'ErrorContextT'.
instance MonadThrow m => MonadErrorContext (ErrorContextT m) where
  errorContextCollect = ErrorContextT ask
  withErrorContext layer (ErrorContextT m) =
    ErrorContextT (local (errorContextPush_ layer) m)

-- | Implement 'MonadThrow' for 'ErrorContextT'.
instance MonadThrow m => MonadThrow (ErrorContextT m) where
  throwM e = do
    errCtx <- errorContextCollect
    lift $ throwM (ErrorWithContext e errCtx)

instance MonadCatch m => MonadCatch (ErrorContextT m) where
  catch (ErrorContextT (ReaderT m)) c =
    ErrorContextT . ReaderT $ \ r -> m r `catch` \ (ErrorWithContext exn _ctx) -> runReaderT (_runErrorContextT (c exn)) r

catchWithContext :: (MonadErrorContext m, MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catchWithContext m handler = catch m handlerWithContext
  where handlerWithContext (ErrorWithContext exn _ctx) = handler exn

-- tryWithContext :: (MonadErrorContext m, MonadCatch m, Exception e) => m a -> m (Either (ErrorWithContext e) a)
-- tryWithContext m handler = undefined

-- | Implement 'MonadResource' for 'ErrorContextT'.
instance (MonadCatch m, MonadResource m) => MonadResource (ErrorContextT m) where
  liftResourceT = lift . liftResourceT

-- | Implement 'MonadReader' for 'ErrorContextT'.
instance MonadReader r m => MonadReader r (ErrorContextT m) where
  ask = ErrorContextT (lift ask)
  local f (ErrorContextT (ReaderT m)) =
    ErrorContextT (ReaderT (\ errCtx -> local f (m errCtx)))

-- | Encapsulates the error context — essentially a stack of 'Text'
-- values.
data ErrorContext = ErrorContext [Text] deriving (Show, Eq)

errorContextPush_ :: Text -> ErrorContext -> ErrorContext
errorContextPush_ layer (ErrorContext layers) =
  ErrorContext (layer : layers)

-- | Boundles an error with an 'ErrorContext'.
data ErrorWithContext e =
  ErrorWithContext e ErrorContext
  deriving (Show)

-- | An @ErrorWithContext e@ can be used as an exception.
instance Exception e => Exception (ErrorWithContext e)

-- | Dump an error with context to stdout.
errorWithContextDump :: (Show e, MonadIO m) => ErrorWithContext e -> m ()
errorWithContextDump (ErrorWithContext err ctx) = do
  liftIO . putStrLn $ "Error: " <> show err
  errorContextDump ctx

  where errorContextDump (ErrorContext layers) = do
          forM_ layers $ \ layer -> do
            liftIO . putStrLn $ "  caused by: " <> Text.unpack layer

-- | Enrich an error with an error context.
errorContextualize
  :: MonadErrorContext m
  => e
  -> m (ErrorWithContext e)
errorContextualize e = do
  errCtx <- errorContextCollect
  pure $ ErrorWithContext e errCtx

-- | Run the provided IO action and rethrow any exceptions thrown by
-- this IO exception as an 'ErrorWithContext' exception.
errorContextualizeIO :: (MonadIO m, MonadCatch m, MonadErrorContext m) => IO a -> m a
errorContextualizeIO m = do
  errCtx <- errorContextCollect
  liftIO $ catchAny m $ \ (SomeException exn) -> throwM (ErrorWithContext exn errCtx)

-- | Forgets the context from an enriched error.
errorContextForget :: ErrorWithContext e -> e
errorContextForget (ErrorWithContext e _errCtx) = e

-- class HasErrorContext r where
--   errorContextEnvPush :: Text -> r -> r
--   errorContextEnvGet :: r -> ErrorContext

-- instance (MonadThrow m, HasErrorContext r) => MonadErrorContext (ReaderT r m) where
--   errorContextCollect = asks errorContextEnvGet
--   withErrorContext layer = local (errorContextEnvPush layer)
