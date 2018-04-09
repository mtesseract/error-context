{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Control.Error.Context.Test (tests) where

import           Control.Error.Context
import           Control.Exception           (Exception (..), throw, throwIO)
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch, catch, throwM, try)
import           Control.Monad.IO.Class
import           Data.Text                   (Text)
import           Katip
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests" $
  [ testGroup "Simple (ErrorContextT)" (testsWithConf testConfSimple)
  , testGroup "Katip (ErrorContextKatipT)" (testsWithConf testConfKatip)
  ]

testsWithConf :: TestConf -> [TestTree]
testsWithConf conf =
    [
      testCase "Contextualize IO Exception"
        (testContextualizeIOException conf)
    , testCase "throwM"
        (testThrow conf)
    , testCase "catchAnyWithContext"
        (testCatchAnyWithContext conf)
    , testCase "catchAnyWithContext/pure"
        (testCatchAnyWithContextPure conf)
    , testCase "catchAnyWithoutContext"
        (testCatchAnyWithoutContext conf)
    , testCase "catchAnyWithoutContext/pure"
        (testCatchAnyWithoutContextPure conf)
    , testCase "Catch context-enriched exception without context"
        (testCatchWithoutContext conf)
    , testCase "Contextualize error value"
        (testContextualizeErrorValue conf)
    , testCase "Forgetting error context"
        (testForgetErrorContext conf)
    , testCase "Dumping error context"
        (testDumpErrorContext conf)
    , testCase "Throw and catch"
        (testThrowAndCatch conf)
    , testCase "Catch non-contextualized exception with context"
        (testNonContextualizedCatchWithContext conf)
    , testCase "ensureExceptionContext"
        (testEnsureExceptionContext conf)
    , testCase "catch head exception"
        (testCatchHeadException conf)
    , testCase "tryAnyWithoutContext"
        (testTryAnyWithoutContext conf)
    , testCase "tryAnyWithoutContext/pure"
        (testTryAnyWithoutContextPure conf)
    , testCase "tryAnyWithContext"
        (testTryAnyWithContext conf)
    , testCase "tryAnyWithContext/pure"
        (testTryAnyWithContextPure conf)
    , testCase "tryWithContext"
        (testTryWithContext conf)
    , testCase "tryWithContext/pure"
        (testTryWithContextPure conf)
    , testCase "tryWithoutContext"
        (testTryWithoutContext conf)
    , testCase "tryWithoutContext/pure"
        (testTryWithoutContextPure conf)
    , testCase "Throw and catch"
        (testThrowAndCatch conf)
    ]

data TestException = TestException deriving (Show, Eq)

instance Exception TestException

data TestConf where
  TestConf :: forall m. (MonadIO m, MonadCatch m, MonadErrorContext m) =>
              { runStackT         :: forall a. m a -> IO a }
           -> TestConf

testConfKatip :: TestConf
testConfKatip =
  TestConf { runStackT = \ m -> do
               logEnv <- liftIO $ initLogEnv "test-suite" "test"
               runKatipContextT logEnv () (Namespace []) $ runErrorContextKatipT m
           }

testConfSimple :: TestConf
testConfSimple =
  TestConf { runStackT = runErrorContextT }

extractNamespace :: ErrorContext -> [Text]
extractNamespace (ErrorContext _kvs namespace) =
  reverse namespace

testContextualizeIOException :: TestConf -> Assertion
testContextualizeIOException TestConf { .. } = do
  Left (ErrorWithContext ctx TestException) <- try . runStackT $
    withErrorContext "A" $
    withErrorContext "B" $
    liftIO failingIOException
  ["B", "A"] @=? extractNamespace ctx

  where failingIOException :: IO ()
        failingIOException =
          throwIO TestException

testThrow :: TestConf -> Assertion
testThrow TestConf { .. } = do
  catch (runStackT (throwM TestException)) $ \ someExn -> do
    let Just (ErrorWithContext _ctx someInnerExn) = fromException someExn
    liftIO $ Just TestException @=? fromException someInnerExn

testCatchWithoutContext :: TestConf -> Assertion
testCatchWithoutContext TestConf { .. } = do
  TestException <- runStackT $
    withErrorContext "A" $
    withErrorContext "B" $
    catchWithoutContext (throwM TestException) $ \ (exn :: TestException) -> do
      pure exn
  pure ()

testContextualizeErrorValue :: TestConf -> Assertion
testContextualizeErrorValue TestConf { .. } = do
  ErrorWithContext ctx TestException <- runStackT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  ["B", "A"] @=? extractNamespace ctx

testForgetErrorContext :: TestConf -> Assertion
testForgetErrorContext TestConf { .. } = do
  errWithCtx @ (ErrorWithContext _ctx TestException) <- runStackT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  TestException @=? errorContextForget errWithCtx

testDumpErrorContext :: TestConf -> Assertion
testDumpErrorContext TestConf { .. } = do
  errWithCtx @ (ErrorWithContext _ctx _exn) <- runStackT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  errorWithContextDump errWithCtx

testThrowAndCatch :: TestConf -> Assertion
testThrowAndCatch TestConf { .. } = do
  void . runStackT $
    catch (throwM TestException) $ \ TestException -> pure ()

testCatchAnyWithContext :: TestConf -> Assertion
testCatchAnyWithContext TestConf { .. } = do
  catchAnyWithContext (runStackT (throwM TestException)) $
    \ (ErrorWithContext _ctx someExn) -> do
      Just TestException @=? fromException someExn

testCatchAnyWithContextPure :: TestConf -> Assertion
testCatchAnyWithContextPure TestConf { .. } = do
  catchAnyWithContext (runStackT (throw TestException)) $
    \ (ErrorWithContext _ctx someExn) -> do
      Just TestException @=? fromException someExn

testCatchAnyWithoutContext :: TestConf -> Assertion
testCatchAnyWithoutContext TestConf { .. } = do
  catchAnyWithoutContext (runStackT (throwM TestException)) $
    \ someExn -> do
      Just TestException @=? fromException someExn

testCatchAnyWithoutContextPure :: TestConf -> Assertion
testCatchAnyWithoutContextPure TestConf { .. } = do
  catchAnyWithoutContext (runStackT (throw TestException)) $
    \ someExn -> do
      Just TestException @=? fromException someExn

testNonContextualizedCatchWithContext :: TestConf -> Assertion
testNonContextualizedCatchWithContext TestConf { .. } = do
  ErrorWithContext ctx TestException <- runStackT $
    withErrorContext "A" $
    withErrorContext "B" $ do
    catchWithContext throwPureException $ \ (exn :: ErrorWithContext TestException) -> do
      pure exn
  [] @=? extractNamespace ctx

  where throwPureException = throw TestException

testEnsureExceptionContext :: TestConf -> Assertion
testEnsureExceptionContext TestConf { .. } = do
  Left someExn <- try . runStackT $
    withErrorContext "A" $
    withErrorContext "B" $ do
    ensureExceptionContext $ do
      throw TestException
  let Just (ErrorWithContext ctx someExnWithoutCtx) = fromException someExn
  Just TestException @=? fromException someExnWithoutCtx
  ["B", "A"] @=? extractNamespace ctx

testCatchHeadException :: TestConf -> Assertion
testCatchHeadException TestConf { .. } = do
  Left errWithCtx <- tryAnyWithContext . runStackT $ do
    withErrorContext "Here I am, calling head on an empty list!" $
      ensureExceptionContext $ seq (head []) (pure ())
  let (ErrorWithContext _ctx _exnWithoutCtx) = errWithCtx
  putStrLn . displayException $ errWithCtx

testTryAnyWithContext :: TestConf -> Assertion
testTryAnyWithContext TestConf { .. } = do
  Left (ErrorWithContext _ctx someExn) <- tryAnyWithContext . runStackT $ do
    void $ throwM TestException
    pure ()
  Just TestException @=? fromException someExn

testTryAnyWithContextPure :: TestConf -> Assertion
testTryAnyWithContextPure TestConf { .. } = do
  Left (ErrorWithContext _ctx someExn) <- tryAnyWithContext . runStackT $
    seq (throw TestException) (pure ())
  Just TestException @=? fromException someExn

testTryAnyWithoutContext :: TestConf -> Assertion
testTryAnyWithoutContext TestConf { .. } = do
  Left someExn <- tryAnyWithoutContext . runStackT $ do
    void $ throwM TestException
    pure ()
  Just TestException @=? fromException someExn

testTryAnyWithoutContextPure :: TestConf -> Assertion
testTryAnyWithoutContextPure TestConf { .. } = do
  Left someExn <- tryAnyWithoutContext . runStackT $
    seq (throw TestException) (pure ())
  Just TestException @=? fromException someExn

testTryWithContext :: TestConf -> Assertion
testTryWithContext TestConf { .. } = do
  Left (ErrorWithContext _ctx exn) <- tryWithContext . runStackT $ do
    void $ throwM TestException
    pure ()
  TestException @=? exn

testTryWithContextPure :: TestConf -> Assertion
testTryWithContextPure TestConf { .. } = do
  Left (ErrorWithContext _ctx exn) <- tryWithContext . runStackT $
    seq (throw TestException) (pure ())
  TestException @=? exn

testTryWithoutContext :: TestConf -> Assertion
testTryWithoutContext TestConf { .. } = do
  Left exn <- tryWithoutContext . runStackT $ do
    void $ throwM TestException
    pure ()
  TestException @=? exn

testTryWithoutContextPure :: TestConf -> Assertion
testTryWithoutContextPure TestConf { .. } = do
  Left exn <- tryWithoutContext . runStackT $
    seq (throw TestException) (pure ())
  TestException @=? exn
