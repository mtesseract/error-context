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
{-# LANGUAGE ScopedTypeVariables        #-}
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
  , tryWithContext
  , tryAnyWithContext
  )
  where

import           Control.Exception.Safe       (SomeException (..), catchAny,
                                               catchJust, try, tryAny)
import           Control.Monad.Catch          (Exception (..), MonadCatch (..),
                                               MonadThrow, throwM)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Either.Combinators      (mapLeft)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text

-- | Monad type class providing contextualized errors.
class (Monad m, MonadThrow m) => MonadErrorContext m where
  errorContextCollect :: m ErrorContext
  withErrorContext    :: Text -> m a -> m a

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
runErrorContextT
  :: ErrorContextT m a
  -> m a
runErrorContextT ctx =
  runReaderT (_runErrorContextT ctx) (ErrorContext [])

instance (MonadCatch m, MonadIO m) => MonadIO (ErrorContextT m) where
  liftIO m = do
    ctx <- errorContextCollect
    lift $ errorContextualizeIO ctx m

-- | Implement 'MonadErrorContext' for 'ErrorContextT'.
instance MonadThrow m => MonadErrorContext (ErrorContextT m) where
  errorContextCollect = ErrorContextT ask
  withErrorContext layer (ErrorContextT m) =
    ErrorContextT (local (errorContextPush layer) m)

-- | Implement 'MonadThrow' for 'ErrorContextT'.
instance MonadThrow m => MonadThrow (ErrorContextT m) where
  throwM e = do
    ctx <- errorContextCollect
    lift $ throwM (ErrorWithContext ctx e)

instance MonadCatch m => MonadCatch (ErrorContextT m) where
  catch (ErrorContextT (ReaderT m)) c =
    ErrorContextT . ReaderT $
    \ r -> m r `catchWithoutContext` \ exn -> runReaderT (_runErrorContextT (c exn)) r

-- Problem here.

catchWithContext
  :: forall a e m
   . (MonadCatch m, Exception e)
  => m a
  -> (ErrorWithContext e -> m a)
  -> m a
catchWithContext m handler = catchJust pre m handler
  where pre someExn =
          case fromException someExn of
            Just exn -> Just exn
            Nothing  -> case fromException someExn of
                          Just exn -> Just (ErrorWithContext (ErrorContext []) exn)
                          Nothing  -> Nothing
catchWithoutContext
  :: forall a e m
   . (MonadCatch m, Exception e)
  => m a
  -> (e -> m a)
  -> m a
catchWithoutContext m handler = catchJust pre m handler
  where pre someExn =
          case fromException someExn of
            Just exn -> Just exn
            Nothing  -> case fromException someExn of
                          Just (ErrorWithContext (ErrorContext []) exn) -> Just exn
                          _  -> Nothing

tryAnyWithContext
  :: (MonadErrorContext m, MonadCatch m)
  => m a
  -> m (Either (ErrorWithContext SomeException) a)
tryAnyWithContext m =
  mapLeft (ErrorWithContext (ErrorContext [])) <$> tryAny m

tryWithContext
  :: (MonadErrorContext m, MonadCatch m, Exception e)
  => m a
  -> m (Either (ErrorWithContext e) a)
tryWithContext m =
  mapLeft (ErrorWithContext (ErrorContext [])) <$> try m

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

errorContextPush
  :: Text
  -> ErrorContext
  -> ErrorContext
errorContextPush layer (ErrorContext layers) =
  ErrorContext (layer : layers)

-- | Boundles an error with an 'ErrorContext'.
data ErrorWithContext e =
  ErrorWithContext ErrorContext e
  deriving (Show)

instance Functor ErrorWithContext where
  fmap f (ErrorWithContext ctx e) = ErrorWithContext ctx (f e)

-- | An @ErrorWithContext e@ can be used as an exception.
instance Exception e => Exception (ErrorWithContext e)

-- | Dump an error with context to stdout.
errorWithContextDump
  :: (Show e, MonadIO m)
  => ErrorWithContext e
  -> m ()
errorWithContextDump (ErrorWithContext ctx err) = do
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
  ctx <- errorContextCollect
  pure $ ErrorWithContext ctx e

errorContextualizeIO
  :: MonadIO m
  => ErrorContext
  -> IO a
  -> m a
errorContextualizeIO ctx m = liftIO $
  catchAny m $ \ (SomeException exn) -> throwM (ErrorWithContext ctx exn)

-- | Forgets the context from an enriched error.
errorContextForget
  :: ErrorWithContext e
  -> e
errorContextForget (ErrorWithContext _ctx e) = e
