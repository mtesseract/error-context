{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Control.Error.Context.Test (tests) where

import Data.Aeson
import           Control.Error.Context
import           Control.Exception           (Exception (..), throw, throwIO, SomeException(..))
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch, catch, throwM, try)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import           Data.Text                   (Text)
import           Katip
import           Test.Tasty
import           Test.Tasty.HUnit
import Data.Proxy
import Data.Monoid
import Data.Typeable
import Control.Applicative
import Data.Maybe

tests :: TestTree
tests = testGroup "Tests" $
  [ testGroup "Simple (ErrorContextT)" (testsWithConf testConfSimple)
  -- , testGroup "Katip (ErrorContextKatipT)" (testsWithConf testConfKatip)
  , testGroup "Test Examples" testExamples
  ]

testExamples :: [TestTree]
testExamples = 
  [ testCase "simpleExample" testExample ]

testsWithConf :: TestConf -> [TestTree]
testsWithConf conf =
    [ testCase "Contextualize IO Exception"
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
    , testCase "contextKvRetrieval"
        (testContextKv conf)
    , testCase "contextKvOverwrite"
        (testContextKvOverwrite conf)
    , testCase "testRethrowKeepsExceptionUnchangedCatchAnyWithoutCtx"
        (testRethrowKeepsExceptionUnchangedCatchAnyWithoutCtx conf)
    -- , testCase "testRethrowKeepsExceptionUnchangedCatchAnyWithCtx"
    --     (testRethrowKeepsExceptionUnchangedCatchAnyWithCtx conf)
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

extractKVs :: ErrorContext -> HashMap Text Value
extractKVs (ErrorContext kvs _namespace) =
  kvs

testContextualizeIOException :: TestConf -> Assertion
testContextualizeIOException TestConf { .. } = do
  Left (ErrorWithContext ctx TestException) <- try . runStackT $
    withErrorNamespace "A" $
    withErrorNamespace "B" $
    liftIO failingIOException
  ["A", "B"] @=? errorContextNamespace ctx

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
    withErrorNamespace "A" $
    withErrorNamespace "B" $
    catchWithoutContext (throwM TestException) $ \ (exn :: TestException) -> do
      pure exn
  pure ()

testContextualizeErrorValue :: TestConf -> Assertion
testContextualizeErrorValue TestConf { .. } = do
  ErrorWithContext ctx TestException <- runStackT $
    withErrorNamespace "A" $
    withErrorNamespace "B" $
    errorContextualize TestException
  ["A", "B"] @=? errorContextNamespace ctx

testForgetErrorContext :: TestConf -> Assertion
testForgetErrorContext TestConf { .. } = do
  errWithCtx @ (ErrorWithContext _ctx TestException) <- runStackT $
    withErrorNamespace "A" $
    withErrorNamespace "B" $
    errorContextualize TestException
  TestException @=? errorContextForget errWithCtx

testDumpErrorContext :: TestConf -> Assertion
testDumpErrorContext TestConf { .. } = do
  errWithCtx @ (ErrorWithContext _ctx _exn) <- runStackT $
    withErrorNamespace "A" $
    withErrorNamespace "B" $
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
    withErrorNamespace "A" $
    withErrorNamespace "B" $ do
    catchWithContext throwPureException $ \ (exn :: ErrorWithContext TestException) -> do
      pure exn
  [] @=? errorContextNamespace ctx

  where throwPureException = throw TestException

testEnsureExceptionContext :: TestConf -> Assertion
testEnsureExceptionContext TestConf { .. } = do
  Left someExn <- try . runStackT $
    withErrorNamespace "A" $
    withErrorNamespace "B" $ do
    ensureExceptionContext $ do
      throw TestException
  let Just (ErrorWithContext ctx someExnWithoutCtx) = fromException someExn
  Just TestException @=? fromException someExnWithoutCtx
  ["A", "B"] @=? errorContextNamespace ctx

testCatchHeadException :: TestConf -> Assertion
testCatchHeadException TestConf { .. } = do
  Left errWithCtx <- tryAnyWithContext . runStackT $ do
    withErrorNamespace "Here I am, calling head on an empty list!" $
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

testContextKv :: TestConf -> Assertion
testContextKv TestConf { .. } = do
  Left (ErrorWithContext ctx TestException) <- tryWithContext . runStackT $
    withErrorContext "ultimate-answer" answer $
    throwM TestException
  HashMap.fromList [("ultimate-answer", toJSON answer)] @=? extractKVs ctx

  where answer :: Int
        answer = 42

testContextKvOverwrite :: TestConf -> Assertion
testContextKvOverwrite TestConf { .. } = do
  Left (ErrorWithContext ctx TestException) <- tryWithContext . runStackT $
    withErrorContext "ultimate-answer" answer $
    withErrorContext "ultimate-answer" answer' $
    throwM TestException
  HashMap.fromList [("ultimate-answer", toJSON answer')] @=? errorContextKVs ctx

  where answer :: Int
        answer = 42

        answer' :: Int
        answer' = 0

testExample :: IO ()
testExample = do
  Left errWithCtx <- tryAnyWithContext . runErrorContextT $
    withErrorNamespace "middle-earth" $
    withErrorNamespace "mordor" $
    withErrorContext "ring-carrier" ("Frodo" :: Text) $
      throwM TestException
  putStrLn . displayException $ errWithCtx

structurallyEq
  :: forall e e' p
   . (Exception e, Exception e', Exception p)
  => Proxy p
  -> e
  -> e'
  -> Bool
structurallyEq _proxy e e' = do
  case (,) <$> (cast e :: Maybe p) <*> (cast e' :: Maybe p) of
    Just (_, _) -> True
    Nothing ->
      case
          (,)
          <$> (cast e :: Maybe SomeException)
          <*> (cast e' :: Maybe SomeException)
        of
          Just (SomeException f, SomeException f') ->
            structurallyEq _proxy f f'
          Nothing ->
            case
                (,)
                <$> (cast e :: Maybe (ErrorWithContext p))
                <*> (cast e' :: Maybe (ErrorWithContext p))
              of
                Just (ErrorWithContext ctx f, ErrorWithContext ctx' f') ->
                  ctx == ctx' && structurallyEq _proxy f f'
                Nothing ->
                  case
                      (,)
                      <$> (cast e :: Maybe (ErrorWithContext SomeException))
                      <*> (cast e' :: Maybe (ErrorWithContext SomeException))
                    of
                      Just (ErrorWithContext ctx (SomeException f), ErrorWithContext ctx' (SomeException f'))
                        -> ctx == ctx' && structurallyEq _proxy f f'
                      Nothing -> False

testExceptionProxy :: Proxy TestException
testExceptionProxy = Proxy

testRethrowKeepsExceptionUnchangedCatchAnyWithCtx :: TestConf -> Assertion
testRethrowKeepsExceptionUnchangedCatchAnyWithCtx TestConf {..} = do
  let referenceExn =
        SomeException (ErrorWithContext (ErrorContext HashMap.empty ["A"]) (SomeException TestException))
  Left (finalExn :: SomeException) <- try . runStackT $
    withErrorNamespace "A" $
    catchAnyWithContext (throwM TestException) $ \exn ->  do
      liftIO . putStrLn . displayException $ exn
      throwM exn

  putStrLn $ displayException finalExn
  assertBool (displayRawExceptions testExceptionProxy referenceExn finalExn)
    $ structurallyEq testExceptionProxy referenceExn finalExn

testRethrowKeepsExceptionUnchangedCatchAnyWithoutCtx :: TestConf -> Assertion
testRethrowKeepsExceptionUnchangedCatchAnyWithoutCtx TestConf {..} = do
  let referenceExn = SomeException
        (ErrorWithContext (ErrorContext HashMap.empty ["A"])
                          (SomeException TestException))
  Left (finalExn :: SomeException) <-
    try
    . runStackT
    $ withErrorNamespace "A"
    $ catchAnyWithoutContext (throwM TestException) throwM

  assertBool (displayRawExceptions testExceptionProxy referenceExn finalExn)
    $ structurallyEq testExceptionProxy referenceExn finalExn

displayRawExceptions
  :: (Exception e, Exception e', Exception p) => Proxy p -> e -> e' -> String
displayRawExceptions proxy e e' =
  displayRawException proxy e <> " vs. " <> displayRawException proxy e'

-- testEnsureExceptionContextThrowM :: TestConf -> Assertion
-- testEnsureExceptionContextThrowM TestConf { .. } = do
--   Left (ErrorWithContext ctx exn) <- tryAnyWithContext . runStackT $ do
--     withErrorNamespace "A" $
--       ensureExceptionContext $
--         throwM Overflow
--   Just Overflow @=? fromException exn
--   ["A"] @=? errorContextNamespace ctx

-- testAsyncExceptionContextEnriched :: TestConf -> Assertion
-- testAsyncExceptionContextEnriched TestConf {..} = do
--   t <- myThreadId
--   v <- newEmptyMVar
--   liftIO . forkIO $ do
--     threadDelay (10 ^ (6 :: Int))
--     liftIO $ throwTo t TestException
--   Left (exn @ (ErrorWithContext ctx someExnWithoutCtx)) <-
--     tryAnyWithContext
--     . runStackT
--     $ withErrorNamespace "A"
--     $ do
--         -- ensureExceptionContext $
--         -- liftIO $ 
--         -- catch (throwM TestException) $ \ (e :: TestException) -> do
--         -- --   undefined
--           -- undefined
--           -- error "hi"
--         -- liftIO $ 
--         --  liftIO $
--         catchAnyWithContext (throwM TestException)
--           $ \ exnWithCtx @ (ErrorWithContext ctx e) -> do
--               liftIO $ putStrLn "********************"
--               ctx' <- errorContextCollect
--               liftIO $ putStrLn $ "Ctx: " ++ errorContextAsString ctx
--               error "End"
--               -- liftIO
--               --   $  putStrLn
--               --   $  "displayRawException: "
--               --   <> displayRawException (Proxy :: Proxy TestException) e
--               -- throwM exnWithCtx

--               -- catch (void $ takeMVar v) $ \ (someExn :: SomeException) -> do
--           --   liftIO $ putStrLn $ show (displayException someExn)
--   putStrLn $ "Caught: " ++ displayRawException (Proxy :: Proxy TestException) exn
--   Just TestException @=? fromException someExnWithoutCtx
--   ["A"] @=? errorContextNamespace ctx

data RawException = SomeExceptionWrapper RawException
  | RealExceptionWithoutCtx String
  | RealExceptionWithCtx RawException
  | forall exn. Exception exn => UnknownException exn

displayRawException
  :: forall e f . (Exception e, Exception f) => Proxy e -> f -> String
displayRawException _proxy exception = go (decomposeRawException exception)
 where
  decomposeRawException :: Exception exn => exn -> RawException
  decomposeRawException exn =
    fromMaybe (UnknownException exn)
      $   (case cast exn :: Maybe SomeException of
            Just (SomeException e) ->
              Just (SomeExceptionWrapper (decomposeRawException e))
            Nothing -> Nothing
          )
      <|> (case cast exn :: Maybe (ErrorWithContext e) of
            Just (ErrorWithContext _ctx e) ->
              Just (RealExceptionWithCtx (decomposeRawException e))
            Nothing -> Nothing
          )
      <|> (case cast exn :: Maybe (ErrorWithContext SomeException) of
            Just (ErrorWithContext _ctx e) ->
              Just (RealExceptionWithCtx (decomposeRawException e))
            Nothing -> Nothing
          )
      <|> RealExceptionWithoutCtx
      .   show
      <$> (cast exn :: Maybe e)

      --   case cast exn :: Maybe (ErrorWithContext e) of
      --   Just e -> RealExceptionWithCtx (show e)
      --   Nothin
      -- case cast exn :: Maybe e of
      --   Just e  -> RealExceptionWithoutCtx (show e)
      --   Nothing -> case cast exn :: Maybe (ErrorWithContext SomeException) of
      --   Just (ErrorWithContext _ctx exnWithoutCtx) ->
      --     RealExceptionWithCtx (decomposeRawException exnWithoutCtx)
      --   Nothing -> UnknownException exn

  go (UnknownException        exn) = show exn
  go (RealExceptionWithCtx    exn) = "ErrorWithContext(" <> go exn <> ")"
  go (RealExceptionWithoutCtx str) = str
  go (SomeExceptionWrapper    exn) = "SomeException(" <> go exn <> ")"
