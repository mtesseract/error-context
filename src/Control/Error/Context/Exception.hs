{-# LANGUAGE ScopedTypeVariables #-}

module Control.Error.Context.Exception where

import           Control.Error.Context.Types

import           Control.Exception              ( SomeException(..) )
import           Control.Monad.Catch            ( catches
                                                , Handler(..)
                                                , MonadThrow(..)
                                                )
import           Control.Monad
import           Control.Monad.Catch            ( Exception(..)
                                                , MonadCatch(..)
                                                )
import           Control.Monad.IO.Class

import           Control.Monad.Trans
import           Data.Typeable

throwWithContext
  :: (Exception e, MonadTrans t, MonadThrow m, MonadErrorContext (t m))
  => e
  -> t m a
throwWithContext exn = case cast exn :: Maybe SomeException of
  Just (SomeException inner) -> throwUnwrappedException inner
  Nothing                    -> throwUnwrappedException exn
 where
  throwUnwrappedException e = if exceptionHasContext e
    then lift $ throwM e
    else do
      ctx <- errorContextCollect
      lift $ throwM (ErrorWithContext ctx e)


tryAnyWithContext
  :: (MonadIO m, MonadCatchRaw m, MonadThrow m)
  => m a
  -> m (Either (ErrorWithContext SomeException) a)
tryAnyWithContext m = catchWithContext (Right `liftM` m) (return . Left)

tryWithContext
  :: (MonadIO m, MonadCatchRaw m, MonadThrow m, Exception e)
  => m a
  -> m (Either (ErrorWithContext e) a)
tryWithContext m = catchWithContext (Right `liftM` m) (return . Left)

-- | Forgets the context from an enriched error.
errorContextForget :: ErrorWithContext e -> e
errorContextForget (ErrorWithContext _ctx e) = e

catchAnyWithContext
  :: (MonadIO m, MonadCatchRaw m, MonadThrow m)
  => m a
  -> (ErrorWithContext SomeException -> m a)
  -> m a
catchAnyWithContext = catchWithContext

catchWithContext
  :: forall a e m
   . (MonadIO m, MonadCatchRaw m, MonadThrow m, Exception e)
  => m a
  -> (ErrorWithContext e -> m a)
  -> m a
catchWithContext m h = catchRaw m $ \(someExn :: SomeException) ->
  case fromException someExn :: Maybe (ErrorWithContext e) of
    Just exnWithCtx -> h exnWithCtx
    Nothing         -> case fromException someExn :: Maybe e of
      Just exnWithoutCtx -> h (ErrorWithContext mempty exnWithoutCtx)
      Nothing            -> throwM someExn
