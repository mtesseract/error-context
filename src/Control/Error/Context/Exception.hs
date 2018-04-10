{-# LANGUAGE ScopedTypeVariables #-}

module Control.Error.Context.Exception where

import           Control.Error.Context.Types

import           Control.Exception.Safe      (SomeException (..), catchJust)
import           Control.Monad
import           Control.Monad.Catch         (Exception (..), MonadCatch (..))

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
