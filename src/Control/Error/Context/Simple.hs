{-|
Module      : Control.Error.Context.Simple
Description : API for enriching errors with contexts
Copyright   : (c) Moritz Clasmeier 2018
License     : BSD3
Maintainer  : mtesseract@silverratio.net
Stability   : experimental
Portability : POSIX
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

module Control.Error.Context.Simple
  ( ErrorContextT(..)
  , runErrorContextT
  )
where

import           Control.Error.Context.Types
import           Data.Aeson

import           Control.Error.Context.Exception
import           Control.Exception.Safe         ( SomeException(..)
                                                , catchAny
                                                )
import           Control.Monad.Catch            ( Exception(..)
                                                , MonadCatch(..)
                                                , MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import qualified Data.HashMap.Strict           as HashMap
import           Data.Text                      ( Text )

-- | Data type implementing 'MonadErrorContext'.
newtype ErrorContextT m a =
  ErrorContextT { _runErrorContextT :: ReaderT ErrorContext m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadTrans
                           , MonadState s
                           , MonadWriter w )

runErrorContextT :: ErrorContextT m a -> m a
runErrorContextT = flip runReaderT mempty . _runErrorContextT

instance (MonadCatch m, MonadIO m) => MonadIO (ErrorContextT m) where
  liftIO m = do
    ctx <- errorContextCollect
    lift $ errorContextualizeIO ctx m

    where errorContextualizeIO ctx io = liftIO $
            catchAny io $ \ (SomeException exn) -> throwM (ErrorWithContext ctx exn)

instance (MonadCatch m) => MonadThrow (ErrorContextT m) where
  throwM e =
    case fromException (toException e) :: Maybe (ErrorWithContext SomeException) of
      Just exnWithCtx ->
        lift $ throwM exnWithCtx
      Nothing -> do
        ctx <- errorContextCollect
        lift $ throwM (ErrorWithContext ctx (SomeException e))

errorNamespacePush :: Text -> ErrorContext -> ErrorContext
errorNamespacePush label ctx =
  let currentNamespace = errorContextNamespace ctx
  in ctx { errorContextNamespace = currentNamespace ++ [label] }

errorContextAdd :: Text -> Value -> ErrorContext -> ErrorContext
errorContextAdd label val ctx =
  let currentKVs = errorContextKVs ctx
  in ctx { errorContextKVs = HashMap.insert label val currentKVs }

instance (MonadCatch m) => MonadErrorContext (ErrorContextT m) where
  errorContextCollect = ErrorContextT ask
  withErrorNamespace layer (ErrorContextT m) =
    ErrorContextT (local (errorNamespacePush layer) m)
  withErrorContext label val (ErrorContextT m) =
    ErrorContextT (local (errorContextAdd label (toJSON val)) m)

instance (MonadCatch m) => MonadCatch (ErrorContextT m) where
  catch (ErrorContextT m) c =
    ErrorContextT $
    m `catchWithoutContext` \ exn -> _runErrorContextT (c exn)

instance (MonadCatch m, MonadResource m) => MonadResource (ErrorContextT m) where
  liftResourceT = liftResourceT

instance MonadReader r m => MonadReader r (ErrorContextT m) where
  ask = ErrorContextT (lift ask)
  local f (ErrorContextT (ReaderT m)) =
    ErrorContextT (ReaderT (\ errCtx -> local f (m errCtx)))

instance (MonadUnliftIO m, MonadCatch m) => MonadUnliftIO (ErrorContextT m) where
  askUnliftIO = do
    env <- ErrorContextT ask
    unlifter <- lift askUnliftIO
    pure $ UnliftIO (\ (ErrorContextT ec) -> unliftIO unlifter (runReaderT ec env))
