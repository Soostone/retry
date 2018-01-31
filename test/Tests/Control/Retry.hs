{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Control.Retry
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception           (AsyncException (..), IOException,
                                              MaskingState (..),
                                              getMaskingState, throwTo)
import           Control.Monad.Catch
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Strict
import           Data.Default.Class          (def)
import           Data.Either
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.LocalTime         ()
import           Data.Typeable
import           System.IO.Error
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Monadic     as QCM
import           Test.Tasty
import           Test.Tasty.HUnit            (testCase, (@?=), assertBool)
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Control.Retry
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Control.Retry"
  [ recoveringTests
  , monoidTests
  , retryStatusTests
  , quadraticDelayTests
  ]


-------------------------------------------------------------------------------
recoveringTests :: TestTree
recoveringTests = testGroup "recovering"
  [ testProperty "recovering test without quadratic retry delay" $ monadicIO $ do
      startTime <- run getCurrentTime
      timeout <- pick . choose $ (0,15)
      retries <- getSmall . getPositive <$> pick arbitrary
      res <- run . try $ recovering
        (constantDelay timeout <> limitRetries retries)
        testHandlers
        (const $ throwM (userError "booo"))
      endTime <- run getCurrentTime
      QCM.assert (isLeftAnd isUserError res)
      let ms' = (fromInteger . toInteger $ (timeout * retries)) / 1000000.0
      QCM.assert (diffUTCTime endTime startTime >= ms')
  , testGroup "exception hierarchy semantics"
      [ testCase "does not catch async exceptions" $ do
          counter <- newTVarIO 0
          done <- newEmptyMVar
          let work = atomically (modifyTVar' counter succ) >> threadDelay 1000000

          tid <- forkIO $
            recoverAll (limitRetries 2) (const work) `finally` putMVar done ()

          atomically (check . (== 1) =<< readTVar counter)
          throwTo tid UserInterrupt

          takeMVar done

          count <- atomically (readTVar counter)
          count @?= 1

      , testCase "recovers from custom exceptions" $ do
          f <- mkFailN Custom1 2
          res <- try $ recovering
            (constantDelay 5000 <> limitRetries 3)
            [const $ Handler $ \ Custom1 -> return shouldRetry]
            f
          (res :: Either Custom1 ()) @?= Right ()


      , testCase "fails beyond policy using custom exceptions" $ do
          f <- mkFailN Custom1 3
          res <- try $ recovering
            (constantDelay 5000 <> limitRetries 2)
            [const $ Handler $ \ Custom1 -> return shouldRetry]
            f
          (res :: Either Custom1 ()) @?= Left Custom1


      , testCase "does not recover from unhandled exceptions" $ do
          f <- mkFailN Custom2 2
          res <- try $ recovering
            (constantDelay 5000 <> limitRetries 5)
            [const $ Handler $ \ Custom1 -> return shouldRetry]
            f
          (res :: Either Custom2 ()) @?= Left Custom2


      , testCase "recovers in presence of multiple handlers" $ do
          f <- mkFailN Custom2 2
          res <- try $ recovering
            (constantDelay 5000 <> limitRetries 5)
            [ const $ Handler $ \ Custom1 -> return shouldRetry
            , const $ Handler $ \ Custom2 -> return shouldRetry ]
            f
          (res :: Either Custom2 ()) @?= Right ()


      , testCase "general exceptions catch specific ones" $ do
          f <- mkFailN Custom2 2
          res <- try $ recovering
            (constantDelay 5000 <> limitRetries 5)
            [ const $ Handler $ \ (_::SomeException) -> return shouldRetry ]
            f
          (res :: Either Custom2 ()) @?= Right ()


      , testCase "(redundant) even general catchers don't go beyond policy" $ do
          f <- mkFailN Custom2 3
          res <- try $ recovering
            (constantDelay 5000 <> limitRetries 2)
            [ const $ Handler $ \ (_::SomeException) -> return shouldRetry ]
            f
          (res :: Either Custom2 ()) @?= Left Custom2


      , testCase "rethrows in presence of failed exception casts" $ do
          f <- mkFailN Custom2 3
          final <- try $ do
            res <- try $ recovering
              (constantDelay 5000 <> limitRetries 2)
              [ const $ Handler $ \ (_::SomeException) -> return shouldRetry ]
              f
            (res :: Either Custom1 ()) @?= Left Custom1
          final @?= Left Custom2
      ]
  ]


-------------------------------------------------------------------------------
monoidTests :: TestTree
monoidTests = testGroup "Policy is a monoid"
  [ testProperty "left identity" $
      propIdentity (\p -> mempty <> p) id
  , testProperty "right identity" $
      propIdentity (\p -> p <> mempty) id
  , testProperty "associativity" $
      propAssociativity (\x y z -> x <> (y <> z)) (\x y z -> (x <> y) <> z)
  ]
  where
    toPolicy = retryPolicy . apply
    propIdentity left right  =
      property $ \a x ->
        let applyPolicy f = getRetryPolicyM (f $ toPolicy a) x
            validRes = maybe True (>= 0)
        in  monadicIO $ do
            l <- liftIO $ applyPolicy left
            r <- liftIO $ applyPolicy right
            if validRes r && validRes l
              then assert (l == r)
              else return ()
    propAssociativity left right  =
      property $ \a b c x ->
        let applyPolicy f = liftIO $ getRetryPolicyM (f (toPolicy a) (toPolicy b) (toPolicy c)) x
        in monadicIO $ do
              res <- (==) <$> applyPolicy left <*> applyPolicy right
              assert res


-------------------------------------------------------------------------------
retryStatusTests :: TestTree
retryStatusTests = testGroup "retry status"
  [ testCase "passes the correct retry status each time" $ do
      let policy = limitRetries 2 <> constantDelay 100
      rses <- gatherStatuses policy
      rsIterNumber <$> rses @?= [0, 1, 2]
      rsCumulativeDelay <$> rses @?= [0, 100, 200]
      rsPreviousDelay <$> rses @?= [Nothing, Just 100, Just 100]
  ]


-------------------------------------------------------------------------------
policyTransformersTests :: TestTree
policyTransformersTests = testGroup "policy transformers"
  [ testProperty "always produces positive delay with positive constants (no rollover)" $ \(Positive delay) ->
      let res = runIdentity (simulatePolicy 1000 (exponentialBackoff delay))
          delays = catMaybes (snd <$> res)
          mnDelay = if null delays
                      then Nothing
                      else Just (minimum delays)
      in case mnDelay of
           Nothing -> property True
           Just n -> counterexample (show n ++ " is not >= 0") (property (n >= 0))
  , testProperty "exponential backoff is always incrementing" $ \(Positive delay) ->
     let res = runIdentity (simulatePolicy 1000 (limitRetriesByDelay maxBound (exponentialBackoff delay)))
         delays = catMaybes (snd <$> res)
     in sort delays === delays .&&. length (group delays) === length delays
  ]


-------------------------------------------------------------------------------
maskingStateTests :: TestTree
maskingStateTests = testGroup "masking state"
  [ testCase "shouldn't change masking state in a recovered action" $ do
      maskingState <- getMaskingState
      final <- try $ recovering def testHandlers $ const $ do
        maskingState' <- getMaskingState
        maskingState' @?= maskingState
        fail "Retrying..."
      assertBool
        ("Expected IOException but didn't get one")
        (isLeft (final :: Either IOException ()))

  , testCase "should mask asynchronous exceptions in exception handlers" $ do
      let checkMaskingStateHandlers =
            [ const $ Handler $ \(_ :: SomeException) -> do
                maskingState <- getMaskingState
                maskingState @?= MaskedInterruptible
                return shouldRetry
            ]
      final <- try $ recovering def checkMaskingStateHandlers $ const $ fail "Retrying..."
      assertBool
        ("Expected IOException but didn't get one")
        (isLeft (final :: Either IOException ()))
  ]


-------------------------------------------------------------------------------
quadraticDelayTests :: TestTree
quadraticDelayTests = testGroup "quadratic delay"
  [ testProperty "recovering test with quadratic retry delay" $ monadicIO $ do
      startTime <- run getCurrentTime
      timeout <- pick . choose $ (0,15)
      retries <- pick . choose $ (0,8)
      res <- run . try $ recovering (exponentialBackoff timeout <> limitRetries retries)
                                [const $ Handler (\(_::SomeException) -> return True)]
                                (const $ throwM (userError "booo"))
      endTime <- run getCurrentTime
      QCM.assert (isLeftAnd isUserError res)
      let tmo = if retries > 0 then timeout * 2 ^ (retries - 1) else 0
      let ms' = ((fromInteger . toInteger $ tmo) / 1000000.0)
      QCM.assert (diffUTCTime endTime startTime >= ms')
  ]

-------------------------------------------------------------------------------
isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False

testHandlers :: [a -> Handler IO Bool]
testHandlers = [const $ Handler (\(_::SomeException) -> return shouldRetry)]


data Custom1 = Custom1 deriving (Eq,Show,Read,Ord,Typeable)
data Custom2 = Custom2 deriving (Eq,Show,Read,Ord,Typeable)


instance Exception Custom1
instance Exception Custom2


instance Arbitrary RetryStatus where
  arbitrary = do
    Positive n <- arbitrary
    Positive d <- arbitrary
    l <- arbitrary `suchThatMaybe` \(Positive n) -> n <= d
    return (defaultRetryStatus { rsIterNumber = n
                               , rsCumulativeDelay = d
                               , rsPreviousDelay = getPositive <$> l})

instance CoArbitrary RetryStatus where
  coarbitrary (RetryStatus a b c) = variant 0 . coarbitrary (a, b, c)


instance Function RetryStatus where
  function = functionMap (\rs -> (rsIterNumber rs, rsCumulativeDelay rs, rsPreviousDelay rs))
                         (\(n, d, l) -> defaultRetryStatus { rsIterNumber = n
                                                           , rsCumulativeDelay = d
                                                           , rsPreviousDelay = l})

-- | Create an action that will fail exactly N times with the given
-- exception and will then return () in any subsequent calls.
mkFailN :: (Num a, Ord a, Exception e) => e -> a -> IO (s -> IO ())
mkFailN e n = do
    r <- newIORef 0
    return $ const $ do
      old <- atomicModifyIORef' r $ \ old -> (old+1, old)
      case old >= n of
        True  -> return ()
        False -> throwM e


gatherStatuses
    :: MonadIO m
    => RetryPolicyM (WriterT [RetryStatus] m)
    -> m [RetryStatus]
gatherStatuses policy = execWriterT $
  retrying policy (\_ _ -> return shouldRetry)
                  (\rs -> tell [rs])



-- | Just makes things a bit easier to follow instead of a magic value
-- of @return True@
shouldRetry :: Bool
shouldRetry = True
