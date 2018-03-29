{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Control.Error.Context.Test (tests) where

import           Control.Error.Context
import           Control.Exception      (Exception (..), SomeException (..),
                                         throwIO)
import           Control.Exception.Safe (tryAny)
import           Control.Monad
import           Control.Monad.Catch    (catch, throwM, try)
import           Control.Monad.IO.Class
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests"
     [ testCase "Contextualize IO Exception" testContextualizeIOException
     , testCase "Contextualize error value" testContextualizeErrorValue
     , testCase "Forgetting error context" testForgetErrorContext
     , testCase "Dumping error context" testDumpErrorContext
     , testCase "Throw and catch" testThrowAndCatch
     ]

data TestException = TestException deriving (Show, Eq)

instance Exception TestException

testContextualizeIOException :: Assertion
testContextualizeIOException = do
  Left (ErrorWithContext TestException (ErrorContext ctx)) <- try . runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    liftIO failingIOException
  ["B", "A"] @=? ctx

  where failingIOException :: IO ()
        failingIOException =
          throwIO TestException

testContextualizeErrorValue :: Assertion
testContextualizeErrorValue = do
  ErrorWithContext TestException (ErrorContext ctx) <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  ["B", "A"] @=? ctx

testForgetErrorContext :: Assertion
testForgetErrorContext = do
  errWithCtx @ (ErrorWithContext TestException _) <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  TestException @=? errorContextForget errWithCtx

testDumpErrorContext :: Assertion
testDumpErrorContext = do
  errWithCtx @ (ErrorWithContext _ _) <- runErrorContextT $
    withErrorContext "A" $
    withErrorContext "B" $
    errorContextualize TestException
  errorWithContextDump errWithCtx

testThrowAndCatch :: Assertion
testThrowAndCatch = do
  void . runErrorContextT $
    catch (throwM TestException) $ \ TestException -> pure ()
