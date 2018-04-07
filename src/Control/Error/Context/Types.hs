{-|
Module      : Control.Error.Context.Types
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

module Control.Error.Context.Types
  ( ErrorContext(..)
  , ErrorWithContext(..)
  , errorContextAsString
  , MonadErrorContext(..)
  ) where

import           Control.Exception
import           Control.Monad.Catch (MonadThrow)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Typeable
import           Katip

-- | Boundles an error with an 'ErrorContext'.
data ErrorWithContext e =
  ErrorWithContext ErrorContext e

instance Show e => Show (ErrorWithContext e) where
  show (ErrorWithContext _ctx e) = show e

instance Functor ErrorWithContext where
  fmap f (ErrorWithContext ctx e) = ErrorWithContext ctx (f e)

data ErrorContext =
  ErrorContext { _errorContextKV        :: LogContexts
               , _errorContextNamespace :: [Text] }

instance Monoid ErrorContext where
  mempty = ErrorContext mempty mempty
  (ErrorContext kvs namespace) `mappend` (ErrorContext kvs' namespace') =
    ErrorContext (kvs <> kvs') (namespace <> namespace')

-- | An @ErrorWithContext e@ can be used as an exception.
instance Exception e => Exception (ErrorWithContext e) where
  toException exn = SomeException exn
  fromException (SomeException someExn) =
    case cast someExn :: Maybe (ErrorWithContext e) of
      Just (exnWithCtx @ (ErrorWithContext _ctx _exn)) ->
        Just exnWithCtx
      Nothing ->
        Nothing
  displayException (ErrorWithContext ctx exn) = do
    "Exception: " <> displayException exn <> "\n" <> errorContextAsString ctx

errorContextAsString :: ErrorContext -> String
errorContextAsString ctx =
  let layers = _errorContextNamespace ctx
  in concat $ map (\ layer -> "  caused by: " <> Text.unpack layer <> "\n") layers

-- | Monad type class providing contextualized errors.
class (Monad m, MonadThrow m) => MonadErrorContext m where
  errorContextCollect :: m ErrorContext     -- ^ Return the current error context.
