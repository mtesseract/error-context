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
  )
  where

import           Control.Exception.Safe       (SomeException (..), catchAny,
                                               catchJust)
import           Control.Monad.Catch          (Exception (..), MonadCatch (..),
                                               MonadThrow, throwM)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import qualified Data.Text                    as Text
import           Data.Typeable
import           Katip

-- | Boundles an error with an 'ErrorContext'.
data ErrorWithContext e =
  ErrorWithContext ErrorContext e

instance Show e => Show (ErrorWithContext e) where
  show (ErrorWithContext _ctx e) = show e

instance Functor ErrorWithContext where
  fmap f (ErrorWithContext ctx e) = ErrorWithContext ctx (f e)

errorContextAsString :: ErrorContext -> String
errorContextAsString ctx =
  let layers = unNamespace (_errorContextNamespace ctx)
  in concat $ map (\ layer -> "  caused by: " <> Text.unpack layer <> "\n") layers

data ErrorContext =
  ErrorContext { _errorContextKV        :: LogContexts
               , _errorContextNamespace :: Namespace }

instance Monoid ErrorContext where
  mempty = ErrorContext mempty mempty
  (ErrorContext kvs namespace) `mappend` (ErrorContext kvs' namespace') =
    ErrorContext (kvs <> kvs') (namespace <> namespace')

--------------------------------------------------------------------------------

-- | Data type implementing 'MonadErrorContext'.
newtype ErrorContextT m a =
  ErrorContextT { _runErrorContextT :: m a
                } deriving ( Functor
                           , Applicative
                           , Monad
                           -- , MonadTrans
                           , MonadState s
                           , MonadWriter w )

instance MonadTrans ErrorContextT where
  lift = ErrorContextT

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
runErrorContextT = _runErrorContextT

instance (MonadCatch m, KatipContext m, MonadIO m, Katip m) => Katip (ErrorContextT m) where
  getLogEnv = ErrorContextT getLogEnv
  localLogEnv f (ErrorContextT m) = ErrorContextT (localLogEnv f m)

instance (MonadCatch m, KatipContext m) => KatipContext (ErrorContextT m) where
  getKatipContext = ErrorContextT getKatipContext
  localKatipContext f (ErrorContextT m) = ErrorContextT (localKatipContext f m)
  getKatipNamespace = ErrorContextT getKatipNamespace
  localKatipNamespace f (ErrorContextT m) = ErrorContextT (localKatipNamespace f m)

instance (KatipContext m, MonadCatch m, MonadIO m) => MonadIO (ErrorContextT m) where
  liftIO m = do
    context   <- lift getKatipContext
    namespace <- lift getKatipNamespace
    let ctx = ErrorContext context namespace
    lift $ errorContextualizeIO ctx m

    where errorContextualizeIO ctx io = liftIO $
            catchAny io $ \ (SomeException exn) -> throwM (ErrorWithContext ctx exn)

-- | Implement 'MonadThrow' for 'ErrorContextT'.
instance (KatipContext m, MonadCatch m) => MonadThrow (ErrorContextT m) where
  throwM e = do
    case fromException (toException e) :: Maybe (ErrorWithContext SomeException) of
      Just exnWithCtx ->
        lift $ throwM exnWithCtx
      Nothing -> do
        ctx <- errorContextCollect
        lift $ throwM (ErrorWithContext ctx (SomeException e))

errorContextCollect
  :: KatipContext m
  => m ErrorContext
errorContextCollect = do
  context   <- getKatipContext
  namespace <- getKatipNamespace
  pure $ ErrorContext context namespace

-- | Implement 'MonadCatch' for 'ErrorContextT'.
instance (KatipContext m, MonadCatch m) => MonadCatch (ErrorContextT m) where
  catch (ErrorContextT m) c =
    ErrorContextT $
    m `catchWithoutContext` \ exn -> _runErrorContextT (c exn)

-- | Like 'catch', but the handler is required to be context-aware. Is
-- also able to catch exceptions of type 'e' (without context).
catchWithContext
  :: (MonadCatch m, Exception e)
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
   . (MonadCatch m, Exception e)
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
instance (KatipContext m, MonadCatch m, MonadResource m) => MonadResource (ErrorContextT m) where
  liftResourceT = liftResourceT

-- | Implement 'MonadReader' for 'ErrorContextT'.
instance MonadReader r m => MonadReader r (ErrorContextT m) where
  ask = ErrorContextT ask
  local f (ErrorContextT m) =
    ErrorContextT (local f m)

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
  :: KatipContext m
  => e
  -> m (ErrorWithContext e)
errorContextualize e = do
  context <- getKatipContext
  namespace <- getKatipNamespace
  let ctx = ErrorContext context namespace
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

ensureExceptionContext :: (MonadCatch m, KatipContext m) => m a -> m a
ensureExceptionContext m =
  catchAny m $ \ someExn ->
  case fromException someExn :: Maybe (ErrorWithContext SomeException) of
    Just exnWithCtx ->
      throwM exnWithCtx
    Nothing -> do
      ctx <- errorContextCollect
      throwM $ ErrorWithContext ctx someExn
