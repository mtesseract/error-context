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
  ( ErrorContext(..)
  , ErrorContextT
  , MonadErrorContext(..)
  , ErrorWithContext(..)
  , runErrorContextT
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

import           Control.Exception.Safe       (SomeException (..), catchAny,
                                               catchJust)
import           Control.Monad.Catch          (Exception (..), MonadCatch (..),
                                               MonadThrow, throwM)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Typeable

-- | Monad type class providing contextualized errors.
class (Monad m, MonadThrow m) => MonadErrorContext m where
  errorContextCollect :: m ErrorContext     -- ^ Return the current error context.
  withErrorContext    :: Text -> m a -> m a -- ^ Execute a monadic action while having the
                                            -- provided 'Text' being pushed to the error context.

-- | Data type implementing 'MonadErrorContext'.
newtype ErrorContextT m a =
  ErrorContextT { _runErrorContextT :: ReaderT ErrorContext m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           , MonadTrans
                           , MonadState s
                           , MonadWriter w )

-- | Boundles an error with an 'ErrorContext'.
data ErrorWithContext e =
  ErrorWithContext ErrorContext e
  deriving (Show)

instance Functor ErrorWithContext where
  fmap f (ErrorWithContext ctx e) = ErrorWithContext ctx (f e)

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

-- | Unwrap an 'ErrorContextT'. Exceptions of type @e@ thrown in the
-- provided action via 'throwM' will cause an @'ErrorWithContext' e@
-- exception to be propagated upwards.
runErrorContextT
  :: ErrorContextT m a
  -> m a
runErrorContextT ctx =
  runReaderT (_runErrorContextT ctx) mempty

instance (MonadCatch m, MonadIO m) => MonadIO (ErrorContextT m) where
  liftIO m = do
    ctx <- errorContextCollect
    lift $ errorContextualizeIO ctx m

    where errorContextualizeIO ctx io = liftIO $
            catchAny io $ \ (SomeException exn) -> throwM (ErrorWithContext ctx exn)

-- | Implement 'MonadErrorContext' for 'ErrorContextT'.
instance MonadThrow m => MonadErrorContext (ErrorContextT m) where
  errorContextCollect = ErrorContextT ask
  withErrorContext layer (ErrorContextT m) =
    ErrorContextT (local (errorContextPush layer) m)

-- | Implement 'MonadThrow' for 'ErrorContextT'.
instance MonadThrow m => MonadThrow (ErrorContextT m) where
  throwM e = do
    case fromException (toException e) :: Maybe (ErrorWithContext SomeException) of
      Just exnWithCtx ->
        lift $ throwM exnWithCtx
      Nothing -> do
        ctx <- errorContextCollect
        lift $ throwM (ErrorWithContext ctx (SomeException e))

-- | Implement 'MonadCatch' for 'ErrorContextT'.
instance MonadCatch m => MonadCatch (ErrorContextT m) where
  catch (ErrorContextT (ReaderT m)) c =
    ErrorContextT . ReaderT $
    \ r -> m r `catchWithoutContext` \ exn -> runReaderT (_runErrorContextT (c exn)) r

-- | Like 'catch', but the handler is required to be context-aware. Is
-- also able to catch exceptions of type 'e' (without context).
catchWithContext
  :: (MonadCatch m, Exception e) -- , MonadErrorContext m)
  => m a
  -> (ErrorWithContext e -> m a)
  -> m a
catchWithContext m handler = catchJust pre m handler
  where pre :: Exception e => SomeException -> Maybe (ErrorWithContext e)
        pre someExn =
          -- First we check if the exception is of the type
          -- 'ErrorWithContext e'. If so, provide it to the handler
          -- directly.
          case fromException someExn of
            Just (ErrorWithContext ctx someExnWithoutCtx :: ErrorWithContext SomeException) ->
              case fromException someExnWithoutCtx of
                Just exn -> Just (ErrorWithContext ctx exn)
                Nothing  -> Nothing
            Nothing  ->
              -- Then we check if the exception is of the type 'e',
              -- (without context). In this case we convert it into an
              -- 'ErrorWithContext e' by adding an empty context and
              -- provide the wrapped exception with context to the
              -- handler.
              case fromException someExn of
                Just exn ->
                  Just (ErrorWithContext mempty exn)
                Nothing ->
                  Nothing

-- | Like 'catch', but the handler is required to be context-unaware.
-- Is also able to catch exceptions with context, in which case the
-- context will be forgotten before the exception will be provided to
-- the handler.
catchWithoutContext
  :: forall a e m
   . (MonadCatch m, Exception e) -- , MonadErrorContext m)
  => m a
  -> (e -> m a)
  -> m a
catchWithoutContext m handler = catchJust pre m handler
  where pre :: SomeException -> Maybe e
        pre someExn =
          -- First we check if the exception is of the type
          -- 'ErrorWithContext e'. In this case we forget the context
          -- and provide the exception without context to the handler.
          case fromException someExn :: Maybe (ErrorWithContext SomeException) of
            Just (ErrorWithContext _ctx someExnWithoutContext) ->
              case fromException someExnWithoutContext :: Maybe e of
                Just exn ->
                  Just exn
                Nothing ->
                  Nothing
            Nothing  ->
              -- Then we check if the exception is of type 'e'. If so,
              -- provide it to the handler directly.
              case fromException someExn :: Maybe e of
                Just exn ->
                  Just exn
                Nothing  ->
                  Nothing

tryAnyWithContext
  :: MonadCatch m
  => m a
  -> m (Either (ErrorWithContext SomeException) a)
tryAnyWithContext m =
  catchWithContext (Right `liftM` m) (return . Left)

tryAnyWithoutContext
  :: MonadCatch m
  => m a
  -> m (Either SomeException a)
tryAnyWithoutContext m =
  catchWithoutContext (Right `liftM` m) (return . Left)

tryWithContext
  :: (MonadCatch m, Exception e)
  => m a
  -> m (Either (ErrorWithContext e) a)
tryWithContext m =
  catchWithContext (Right `liftM` m) (return . Left)

tryWithoutContext
  :: (MonadCatch m, Exception e)
  => m a
  -> m (Either e a)
tryWithoutContext m =
  catchWithoutContext (Right `liftM` m) (return . Left)

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

instance Monoid ErrorContext where
  mempty = ErrorContext []
  (ErrorContext c1) `mappend` (ErrorContext c2) = ErrorContext (c1 <> c2)

-- | Push a context layer (a 'Text') onto the provided 'ErrorContext'.
errorContextPush
  :: Text
  -> ErrorContext
  -> ErrorContext
errorContextPush layer (ErrorContext layers) =
  ErrorContext (layer : layers)

errorContextAsString :: ErrorContext -> String
errorContextAsString (ErrorContext layers) =
  concat $ map (\ layer -> "  caused by: " <> Text.unpack layer <> "\n") layers

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

-- | Forgets the context from an enriched error.
errorContextForget
  :: ErrorWithContext e
  -> e
errorContextForget (ErrorWithContext _ctx e) = e

-- | Context aware version of 'catchAny'.
catchAnyWithContext
  :: MonadCatch m
  => m a
  -> (ErrorWithContext SomeException -> m a)
  -> m a
catchAnyWithContext m handler = catchJust pre m handler
  where pre :: SomeException -> Maybe (ErrorWithContext SomeException)
        pre someExn =
          case fromException someExn :: Maybe (ErrorWithContext SomeException) of
            Just exn ->
              Just exn
            Nothing ->
              Just (ErrorWithContext mempty someExn)

-- | Context aware version of 'catchAny'.
catchAnyWithoutContext
  :: MonadCatch m
  => m a
  -> (SomeException -> m a)
  -> m a
catchAnyWithoutContext m handler = catchJust pre m handler
  where pre :: SomeException -> Maybe SomeException
        pre someExn =
          case fromException someExn :: Maybe (ErrorWithContext SomeException) of
            Just (ErrorWithContext _ctx exnWithoutContext) ->
              Just exnWithoutContext
            Nothing ->
              Just someExn

ensureExceptionContext :: (MonadCatch m, MonadErrorContext m) => m a -> m a
ensureExceptionContext m =
  catchAny m $ \ someExn ->
  case fromException someExn :: Maybe (ErrorWithContext SomeException) of
    Just exnWithCtx ->
      throwM exnWithCtx
    Nothing -> do
      ctx <- errorContextCollect
      throwM $ ErrorWithContext ctx someExn
