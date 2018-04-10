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
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Error.Context
  ( MonadErrorContext(..)
  , ErrorContext(..)
  , ErrorContextT
  , runErrorContextT
  , ErrorContextKatipT(..)
  , ErrorWithContext(..)
  , errorContextualize
  , errorContextForget
  , errorWithContextDump
  , catchWithoutContext
  , catchWithContext
  , catchAnyWithContext
  , catchAnyWithoutContext
  , ensureExceptionContext
  , tryAnyWithContext
  , tryAnyWithoutContext
  , tryWithContext
  , tryWithoutContext
  ) where

import           Control.Error.Context.Katip
import           Control.Error.Context.Simple
import           Control.Error.Context.Types

import           Control.Error.Context.Exception
import           Control.Exception.Safe          (SomeException (..), catchAny)
import           Control.Monad.Catch             (Exception (..),
                                                  MonadCatch (..), throwM)
import           Control.Monad.IO.Class
import           Data.Monoid

--------------------------------------------------------------------------------

-- | Dump an error with context to stdout.
errorWithContextDump
  :: (Show e, MonadIO m)
  => ErrorWithContext e
  -> m ()
errorWithContextDump (ErrorWithContext ctx err) = do
  liftIO . putStrLn $ "Error: " <> show err
  liftIO . putStrLn . errorContextAsString $ ctx

-- | Enrich an error value with an error context.
errorContextualize
  :: MonadErrorContext m
  => e
  -> m (ErrorWithContext e)
errorContextualize e = do
  ctx <- errorContextCollect
  pure $ ErrorWithContext ctx e

ensureExceptionContext
  :: (MonadCatch m, MonadErrorContext m)
  => m a
  -> m a
ensureExceptionContext m =
  catchAny m $ \ someExn ->
  case fromException someExn :: Maybe (ErrorWithContext SomeException) of
    Just exnWithCtx ->
      throwM exnWithCtx
    Nothing -> do
      ctx <- errorContextCollect
      throwM $ ErrorWithContext ctx someExn
