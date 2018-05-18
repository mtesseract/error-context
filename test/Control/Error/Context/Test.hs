{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Control.Error.Context.Test where

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
  , testGroup "Katip (ErrorContextKatipT)" (testsWithConf testConfKatip)
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
    , testCase "tryAnyWithContext"
        (testTryAnyWithContext conf)
    , testCase "tryAnyWithContext/pure"
        (testTryAnyWithContextPure conf)
    , testCase "tryWithContext"
        (testTryWithContext conf)
    , testCase "tryWithContext/pure"
        (testTryWithContextPure conf)
    , testCase "try"
        (testTry conf)
    , testCase "try/pure"
        (testTryPure conf)
    , testCase "Throw and catch"
        (testThrowAndCatch conf)
    , testCase "contextKvRetrieval"
        (testContextKv conf)
    , testCase "contextKvOverwrite"
        (testContextKvOverwrite conf)
    , testCase "catchAnyWithContextRethrow"
        (testCatchAnyWithContextRethrow conf)
    , testCase "catchWithContextRethrow"
        (testCatchWithContextRethrow conf)
    , testCase "catchRethrow"
        (testCatchRethrow conf)
    ]

data TestException = TestException deriving (Show, Eq)

instance Exception TestException

data TestConf where
  TestConf :: forall m. (MonadIO m, MonadCatch m, MonadCatchRaw m, MonadErrorContext m) =>
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
    catch (throwM TestException) $ \ (exn :: TestException) -> do
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

-- testCatchAnyWithContext :: TestConf -> Assertion
-- testCatchAnyWithContext TestConf { .. } = do
--   catchAnyWithContext (runStackT (throwM TestException)) $
--     \ (ErrorWithContext _ctx someExn) -> do
--       Just TestException @=? fromException someExn

-- testCatchAnyWithContextPure :: TestConf -> Assertion
-- testCatchAnyWithContextPure TestConf { .. } = do
--   catchAnyWithContext (runStackT (throw TestException)) $
--     \ (ErrorWithContext _ctx someExn) -> do
--       Just TestException @=? fromException someExn

-- testCatchAnyWithoutContext :: TestConf -> Assertion
-- testCatchAnyWithoutContext TestConf { .. } = do
--   catchAnyWithoutContext (runStackT (throwM TestException)) $
--     \ someExn -> do
--       Just TestException @=? fromException someExn

-- testCatchAnyWithoutContextPure :: TestConf -> Assertion
-- testCatchAnyWithoutContextPure TestConf { .. } = do
--   catchAnyWithoutContext (runStackT (throw TestException)) $
--     \ someExn -> do
--       Just TestException @=? fromException someExn

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
  Left errWithCtx :: Either (ErrorWithContext SomeException) () <- try . runStackT $ do
    withErrorNamespace "Here I am, calling head on an empty list!" $
      ensureExceptionContext $ seq (head []) (pure ())
  let (ErrorWithContext _ctx _exnWithoutCtx) = errWithCtx
  putStrLn . displayException $ errWithCtx

testTryAnyWithContext :: TestConf -> Assertion
testTryAnyWithContext TestConf { .. } = do
  Left (ErrorWithContext _ctx someExn) :: Either (ErrorWithContext SomeException) () <- try . runStackT $ do
    void $ throwM TestException
    pure ()
  Just TestException @=? fromException someExn

testTryAnyWithContextPure :: TestConf -> Assertion
testTryAnyWithContextPure TestConf { .. } = do
  Left (ErrorWithContext _ctx someExn) :: Either (ErrorWithContext SomeException) () <- try . runStackT $
    ensureExceptionContext $
      seq (throw TestException) (pure ())
  Just TestException @=? fromException someExn

testTryWithContext :: TestConf -> Assertion
testTryWithContext TestConf { .. } = do
  Left (ErrorWithContext _ctx exn) :: Either (ErrorWithContext TestException) () <- try . runStackT $ do
    void $ throwM TestException
    pure ()
  TestException @=? exn

testTryWithContextPure :: TestConf -> Assertion
testTryWithContextPure TestConf { .. } = do
  Left (ErrorWithContext _ctx exn) :: Either (ErrorWithContext TestException) () <- try . runStackT $
    ensureExceptionContext $
      seq (throw TestException) (pure ())
  TestException @=? exn

testTry :: TestConf -> Assertion
testTry TestConf { .. } = do
  Left (ErrorWithContext _ctx exn) <- try . runStackT $ do
    void $ throwM TestException
    pure ()
  TestException @=? exn

testTryPure :: TestConf -> Assertion
testTryPure TestConf { .. } = do
  Left exn <- try . runStackT $
    seq (throw TestException) (pure ())
  TestException @=? exn

testContextKv :: TestConf -> Assertion
testContextKv TestConf { .. } = do
  Left (ErrorWithContext ctx TestException) :: Either (ErrorWithContext TestException) () <- try . runStackT $
    withErrorContext "ultimate-answer" answer $
    throwM TestException
  HashMap.fromList [("ultimate-answer", toJSON answer)] @=? extractKVs ctx

  where answer :: Int
        answer = 42

testContextKvOverwrite :: TestConf -> Assertion
testContextKvOverwrite TestConf { .. } = do
  Left (ErrorWithContext ctx TestException) :: Either (ErrorWithContext TestException) () <- try . runStackT $
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
  Left errWithCtx :: Either (ErrorWithContext SomeException) () <- try . runErrorContextT $
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

testCatchAnyWithContextRethrow :: TestConf -> Assertion
testCatchAnyWithContextRethrow TestConf {..} = do
  let referenceExn =
        SomeException (ErrorWithContext (ErrorContext HashMap.empty ["A"]) (SomeException TestException))
  Left (finalExn :: SomeException) <- try . runStackT $
    withErrorNamespace "A" $
    catchWithContext (throwM TestException) $ \ (ErrorWithContext _ctx (exn:: SomeException)) ->  do
      throwM exn

  assertBool (displayRawExceptions testExceptionProxy referenceExn finalExn)
    $ structurallyEq testExceptionProxy referenceExn finalExn

testCatchWithContextRethrow :: TestConf -> Assertion
testCatchWithContextRethrow TestConf {..} = do
  let referenceExn =
        SomeException (ErrorWithContext (ErrorContext HashMap.empty ["A"]) (SomeException TestException))
  Left (finalExn :: SomeException) <- try . runStackT $
    withErrorNamespace "A" $
    catchWithContext (throwM TestException) $ \exn@(ErrorWithContext _ctx TestException) ->  do
      throwM exn

  assertBool (displayRawExceptions testExceptionProxy referenceExn finalExn)
    $ structurallyEq testExceptionProxy referenceExn finalExn

testCatchRethrow :: TestConf -> Assertion
testCatchRethrow TestConf {..} = do
  let referenceExn = SomeException (ErrorWithContext (ErrorContext HashMap.empty ["A"]) (SomeException TestException))
  Left (finalExn :: SomeException) <-
    try
    . runStackT
    $ withErrorNamespace "A"
    $ catch (throwM TestException) (\exn -> throwM (exn :: TestException))

  assertBool (displayRawExceptions testExceptionProxy referenceExn finalExn)
    $ structurallyEq testExceptionProxy referenceExn finalExn

displayRawExceptions
  :: (Exception e, Exception e', Exception p) => Proxy p -> e -> e' -> String
displayRawExceptions proxy e e' =
  displayRawException proxy e <> " vs. " <> displayRawException proxy e'

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

displayRawTestException
  :: Exception e => e -> String
displayRawTestException = displayRawException (Proxy :: Proxy TestException)

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

  go (UnknownException        exn) = show exn
  go (RealExceptionWithCtx    exn) = "ErrorWithContext(" <> go exn <> ")"
  go (RealExceptionWithoutCtx str) = str
  go (SomeExceptionWrapper    exn) = "SomeException(" <> go exn <> ")"
