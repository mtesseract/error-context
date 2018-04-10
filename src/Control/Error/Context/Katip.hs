{-|
Module      : Control.Error.Context.Katip
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
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Error.Context.Katip (ErrorContextKatipT(..)) where

import           Control.Error.Context.Exception
import           Control.Error.Context.Types
import           Control.Exception.Safe          (SomeException (..), catchAny)
import           Control.Monad.Catch             (Exception (..),
                                                  MonadCatch (..), MonadThrow,
                                                  throwM)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Katip

-- | Data type implementing 'MonadErrorContext'.
newtype ErrorContextKatipT m a =
  ErrorContextKatipT { runErrorContextKatipT :: m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           -- , MonadTrans
                           , MonadState s
                           , MonadWriter w )

instance MonadTrans ErrorContextKatipT where
  lift = ErrorContextKatipT

instance (MonadCatch m, KatipContext m, MonadIO m, Katip m) => Katip (ErrorContextKatipT m) where
  getLogEnv = ErrorContextKatipT getLogEnv
  localLogEnv f (ErrorContextKatipT m) = ErrorContextKatipT (localLogEnv f m)

instance (MonadCatch m, KatipContext m) => KatipContext (ErrorContextKatipT m) where
  getKatipContext = ErrorContextKatipT getKatipContext
  localKatipContext f (ErrorContextKatipT m) = ErrorContextKatipT (localKatipContext f m)
  getKatipNamespace = ErrorContextKatipT getKatipNamespace
  localKatipNamespace f (ErrorContextKatipT m) = ErrorContextKatipT (localKatipNamespace f m)

instance (KatipContext m, MonadCatch m, MonadIO m) => MonadIO (ErrorContextKatipT m) where
  liftIO m = do
    context   <- toObject <$> lift getKatipContext
    namespace <- lift getKatipNamespace
    let ctx = ErrorContext context (unNamespace namespace)
    lift $ errorContextualizeIO ctx m

    where errorContextualizeIO ctx io = liftIO $
            catchAny io $ \ (SomeException exn) -> throwM (ErrorWithContext ctx exn)

instance (KatipContext m, MonadCatch m) => MonadThrow (ErrorContextKatipT m) where
  throwM e = do
    case fromException (toException e) :: Maybe (ErrorWithContext SomeException) of
      Just exnWithCtx ->
        lift $ throwM exnWithCtx
      Nothing -> do
        ctx <- errorContextCollect
        lift $ throwM (ErrorWithContext ctx (SomeException e))

instance (MonadCatch m, KatipContext m) => MonadErrorContext (ErrorContextKatipT m) where
  errorContextCollect = do
    context   <- toObject <$> lift getKatipContext
    namespace <- lift getKatipNamespace
    pure $ ErrorContext context (unNamespace namespace)
  withErrorNamespace label =
    katipAddNamespace (Namespace [label])
  withErrorContext label val =
    katipAddContext (sl label val)

instance (KatipContext m, MonadCatch m) => MonadCatch (ErrorContextKatipT m) where
  catch (ErrorContextKatipT m) c =
    ErrorContextKatipT $
    m `catchWithoutContext` \ exn -> runErrorContextKatipT (c exn)

instance (KatipContext m, MonadCatch m, MonadResource m) => MonadResource (ErrorContextKatipT m) where
  liftResourceT = liftResourceT

instance MonadReader r m => MonadReader r (ErrorContextKatipT m) where
  ask = ErrorContextKatipT ask
  local f (ErrorContextKatipT m) =
    ErrorContextKatipT (local f m)
