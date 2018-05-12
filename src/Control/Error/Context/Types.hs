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
  , MonadCatchRaw(..)
  , ErrorWithContext(..)
  , errorContextAsString
  , exceptionHasContext
  , MonadErrorContext(..)
  )
where

import           Control.Exception
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Aeson
import qualified Data.ByteString.Lazy          as ByteString.Lazy
import           Data.Function                  ( (&) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Monoid
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Typeable

-- | Boundles an error with an 'ErrorContext'.
data ErrorWithContext e =
  ErrorWithContext ErrorContext e
  deriving (Typeable)

instance Show e => Show (ErrorWithContext e) where
  show (ErrorWithContext _ctx e) = show e

instance Functor ErrorWithContext where
  fmap f (ErrorWithContext ctx e) = ErrorWithContext ctx (f e)

instance Applicative ErrorWithContext where
  pure = ErrorWithContext mempty
  (ErrorWithContext ctx f) <*> (ErrorWithContext ctx' a) = ErrorWithContext (ctx <> ctx') (f a)

data ErrorContext =
  ErrorContext { errorContextKVs       :: HashMap Text Value
               , errorContextNamespace :: [Text] }
  deriving (Eq)

instance Monoid ErrorContext where
  mempty = ErrorContext mempty mempty
  (ErrorContext kvs namespace) `mappend` (ErrorContext kvs' namespace') =
    ErrorContext (kvs <> kvs') (namespace <> namespace')

-- | An @ErrorWithContext e@ can be used as an exception.
instance Exception e => Exception (ErrorWithContext e) where
  toException (ErrorWithContext ctx exn) = SomeException (ErrorWithContext ctx (toException exn))
  fromException (SomeException someExn) =
    case cast someExn :: Maybe (ErrorWithContext SomeException) of
      Just (ErrorWithContext ctx someExnWithoutCtx) ->
        case fromException someExnWithoutCtx :: Maybe e of
          Just exnWithoutCtx ->
            Just (ErrorWithContext ctx exnWithoutCtx)
          Nothing ->
            Nothing
      Nothing ->
        Nothing
  displayException (ErrorWithContext ctx exn) =
    "Exception: " <> displayException exn <> "\n"
    <> errorContextAsString ctx

errorContextAsString :: ErrorContext -> String
errorContextAsString (ErrorContext hashmap layers) =
  concat $ prettyPrintKvs ++ prettyPrintCauses
 where
  prettyPrintKvs =
    hashmap
      & HashMap.toList
      & (map $ \(label, val) ->
          let
            labelS = label & Text.unpack
            valS =
              val
                & encode
                & ByteString.Lazy.toStrict
                & Text.decodeUtf8
                & Text.unpack
          in
            "           " <> labelS <> ": " <> valS <> "\n"
        )

  prettyPrintCauses =
    layers
      & reverse
      & (map $ \layer ->
          let layerS = Text.unpack layer in "  caused by: " <> layerS <> "\n"
        )
-- | Monad type class providing contextualized errors.
class (Monad m, MonadThrow m) => MonadErrorContext m where
  errorContextCollect :: m ErrorContext     -- ^ Return the current error context.
  withErrorContext :: ToJSON v => Text -> v -> m a -> m a
  withErrorNamespace :: Text -> m a -> m a

-- exceptionHasContext :: Exception e => e -> Bool
-- exceptionHasContext exn =
--   case cast exn :: Maybe SomeException of
--     Just (SomeException e) -> exceptionHasContext e
--     Nothing -> let rep  = typeOf exn
--                    con  = typeRepTyCon rep
--                    con' = typeRepTyCon (typeOf (ErrorWithContext mempty ()))
--                 in con == con'

exceptionHasContext :: Exception e => e -> Bool
exceptionHasContext exn =
  let rep  = typeOf exn
      con  = typeRepTyCon rep
      con' = typeRepTyCon (typeOf (ErrorWithContext mempty ()))
  in  con == con'

class MonadCatchRaw m where
  catchRaw :: Exception e => m a -> (e -> m a) -> m a

