{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Control.Error.Context.Test (tests) where

import           Control.Error.Context
import           Control.Exception      (Exception (..), throw, throwIO)
import           Control.Monad
import           Control.Monad.Catch    (catch, throwM, try)
import           Control.Monad.IO.Class
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [
      testCase "Contextualize IO Exception"
        testContextualizeIOException
    , testCase "throwM"
        testThrow
    , testCase "catchAnyWithContext"
        testCatchAnyWithContext
    , testCase "catchAnyWithContext/pure"
        testCatchAnyWithContextPure
    , testCase "catchAnyWithoutContext"
        testCatchAnyWithoutContext
    , testCase "catchAnyWithoutContext/pure"
        testCatchAnyWithoutContextPure
    , testCase "Catch context-enriched exception without context"
        testCatchWithoutContext
    , testCase "Contextualize error value"
        testContextualizeErrorValue
    , testCase "Forgetting error context"
        testForgetErrorContext
    , testCase "Dumping error context"
        testDumpErrorContext
    , testCase "Throw and catch"
        testThrowAndCatch
    , testCase "Catch non-contextualized exception with context"
        testNonContextualizedCatchWithContext
    , testCase "ensureExceptionContext"
        testEnsureExceptionContext
    , testCase "catch head exception"
        testCatchHeadException
    , testCase "tryAnyWithoutContext"
        testTryAnyWithoutContext
    , testCase "tryAnyWithoutContext/pure"
        testTryAnyWithoutContextPure
    , testCase "tryAnyWithContext"
        testTryAnyWithContext
    , testCase "tryAnyWithContext/pure"
        testTryAnyWithContextPure
    , testCase "tryWithContext"
        testTryWithContext
    , testCase "tryWithContext/pure"
        testTryWithContextPure
    , testCase "tryWithoutContext"
        testTryWithoutContext
    , testCase "tryWithoutContext/pure"
        testTryWithoutContextPure
    , testCase "Throw and catch"
        testThrowAndCatch
    ]

data TestException = TestException deriving (Show, Eq)

instance Exception TestException

testContextualizeIOException :: Assertion
testContextualizeIOException = do
  Left (ErrorWithContext (ErrorContext ctx) TestException) <- try . runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    liftIO failingIOException
  ["B", "A"] @=? ctx

  where failingIOException :: IO ()
        failingIOException =
          throwIO TestException

testCatchWithoutContext :: Assertion
testCatchWithoutContext = do
  TestException <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    catchWithoutContext (throwM TestException) $ \ (exn :: TestException) -> do
      pure exn
  pure ()

testContextualizeErrorValue :: Assertion
testContextualizeErrorValue = do
  ErrorWithContext (ErrorContext ctx) TestException <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  ["B", "A"] @=? ctx

testForgetErrorContext :: Assertion
testForgetErrorContext = do
  errWithCtx @ (ErrorWithContext _ctx TestException) <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  TestException @=? errorContextForget errWithCtx

testDumpErrorContext :: Assertion
testDumpErrorContext = do
  errWithCtx @ (ErrorWithContext _ctx _exn) <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  errorWithContextDump errWithCtx

testThrowAndCatch :: Assertion
testThrowAndCatch = do
  void . runErrorContextT $
    catch (throwM TestException) $ \ TestException -> pure ()

testThrow :: Assertion
testThrow = do
  catch (runErrorContextT (throwM TestException)) $ \ someExn -> do
    let Just (ErrorWithContext _ctx someInnerExn) = fromException someExn
    liftIO $ Just TestException @=? fromException someInnerExn

testCatchAnyWithContext :: Assertion
testCatchAnyWithContext = do
  catchAnyWithContext (runErrorContextT (throwM TestException)) $
    \ (ErrorWithContext _ctx someExn) -> do
      Just TestException @=? fromException someExn

testCatchAnyWithContextPure :: Assertion
testCatchAnyWithContextPure = do
  catchAnyWithContext (runErrorContextT (throw TestException)) $
    \ (ErrorWithContext _ctx someExn) -> do
      Just TestException @=? fromException someExn

testCatchAnyWithoutContext :: Assertion
testCatchAnyWithoutContext = do
  catchAnyWithoutContext (runErrorContextT (throwM TestException)) $
    \ someExn -> do
      Just TestException @=? fromException someExn

testCatchAnyWithoutContextPure :: Assertion
testCatchAnyWithoutContextPure = do
  catchAnyWithoutContext (runErrorContextT (throw TestException)) $
    \ someExn -> do
      Just TestException @=? fromException someExn

testNonContextualizedCatchWithContext :: Assertion
testNonContextualizedCatchWithContext = do
  ErrorWithContext (ErrorContext ctx) TestException <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $ do
    catchWithContext throwPureException $ \ (exn :: ErrorWithContext TestException) -> do
      pure exn
  [] @=? ctx

  where throwPureException = throw TestException

testEnsureExceptionContext :: Assertion
testEnsureExceptionContext = do
  Left someExn <- try . runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $ do
    ensureExceptionContext $ do
      throw TestException
  let Just (ErrorWithContext ctx someExnWithoutCtx) = fromException someExn
  Just TestException @=? fromException someExnWithoutCtx
  ErrorContext ["B", "A"] @=? ctx

testCatchHeadException :: Assertion
testCatchHeadException = do
  Left errWithCtx <- tryAnyWithContext . runErrorContextT $ do
    withErrorContext "Here I am, calling head on an empty list!" $
      ensureExceptionContext $ seq (head []) (pure ())
  let (ErrorWithContext _ctx _exnWithoutCtx) = errWithCtx
  putStrLn . displayException $ errWithCtx

testTryAnyWithContext :: Assertion
testTryAnyWithContext = do
  Left (ErrorWithContext _ctx someExn) <- tryAnyWithContext . runErrorContextT $ do
    void $ throwM TestException
    pure ()
  Just TestException @=? fromException someExn

testTryAnyWithContextPure :: Assertion
testTryAnyWithContextPure = do
  Left (ErrorWithContext _ctx someExn) <- tryAnyWithContext . runErrorContextT $
    seq (throw TestException) (pure ())
  Just TestException @=? fromException someExn

testTryAnyWithoutContext :: Assertion
testTryAnyWithoutContext = do
  Left someExn <- tryAnyWithoutContext . runErrorContextT $ do
    void $ throwM TestException
    pure ()
  Just TestException @=? fromException someExn

testTryAnyWithoutContextPure :: Assertion
testTryAnyWithoutContextPure = do
  Left someExn <- tryAnyWithoutContext . runErrorContextT $
    seq (throw TestException) (pure ())
  Just TestException @=? fromException someExn

testTryWithContext :: Assertion
testTryWithContext = do
  Left (ErrorWithContext _ctx exn) <- tryWithContext . runErrorContextT $ do
    void $ throwM TestException
    pure ()
  TestException @=? exn

testTryWithContextPure :: Assertion
testTryWithContextPure = do
  Left (ErrorWithContext _ctx exn) <- tryWithContext . runErrorContextT $
    seq (throw TestException) (pure ())
  TestException @=? exn

testTryWithoutContext :: Assertion
testTryWithoutContext = do
  Left exn <- tryWithoutContext . runErrorContextT $ do
    void $ throwM TestException
    pure ()
  TestException @=? exn

testTryWithoutContextPure :: Assertion
testTryWithoutContextPure = do
  Left exn <- tryWithoutContext . runErrorContextT $
    seq (throw TestException) (pure ())
  TestException @=? exn

-- testTryAnyWithoutContext :: Assertion
-- testTryAnyWithoutContext = do
--   Left someExn <- tryAnyWithoutContext . runErrorContextT $ do
--     void $ throwM TestException
--     pure ()
--   Just TestException @=? fromException someExn
