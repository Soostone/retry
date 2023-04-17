{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Tests.Control.Retry
    ( tests

    -- * Used to test UnliftIO versions of the same functions
    , recoveringTestsWith
    , maskingStateTestsWith
    , quadraticDelayTestsWith
    , recoveringTest
    , testHandlers
    , testHandlersDynamic
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM      as STM
import qualified Control.Exception           as EX
import           Control.Monad               ( forM_ )
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.IO.Class      as MIO
import           Control.Monad.Writer.Strict
import           Data.Either
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.LocalTime         ()
import           Data.Typeable
import           Hedgehog                    as HH
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           System.IO.Error
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit            ( assertBool, assertFailure
                                             , testCase, (@=?), (@?=)
                                             )
-------------------------------------------------------------------------------
import           Control.Retry
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Control.Retry"
  [ recoveringTests
  , monoidTests
  , retryStatusTests
  , quadraticDelayTests
  , policyTransformersTests
  , maskingStateTests
  , capDelayTests
  , limitRetriesByCumulativeDelayTests
  , overridingDelayTests
  , resumableTests
  , retryOnErrorTests
  ]


-------------------------------------------------------------------------------
recoveringTests :: TestTree
recoveringTests = recoveringTestsWith recovering


recoveringTestsWith
    :: Monad m
    => (RetryPolicyM m -> [RetryStatus -> Handler IO Bool] -> (a -> IO ()) -> IO ())
    -> TestTree
recoveringTestsWith recovering' = testGroup "recovering"
  [ testProperty "recovering test without quadratic retry delay" $ property $ do
      startTime <- liftIO getCurrentTime
      timeout <- forAll (Gen.int (Range.linear 0 15))
      retries <- forAll (Gen.int (Range.linear 0 50))
      res <- liftIO $ try $ recovering'
        (constantDelay timeout <> limitRetries retries)
        testHandlers
        (const $ throwM (userError "booo"))
      endTime <- liftIO getCurrentTime
      HH.assert (isLeftAnd isUserError res)
      let ms' = (fromInteger . toInteger $ (timeout * retries)) / 1000000.0
      HH.assert (diffUTCTime endTime startTime >= ms')
  , testGroup "exception hierarchy semantics"
      [ testCase "does not catch async exceptions" $ do
          counter <- newTVarIO (0 :: Int)
          done <- newEmptyMVar
          let work = atomically (modifyTVar' counter succ) >> threadDelay 1000000

          tid <- forkIO $
            recoverAll (limitRetries 2) (const work) `finally` putMVar done ()

          atomically (STM.check . (== 1) =<< readTVar counter)
          EX.throwTo tid EX.UserInterrupt

          takeMVar done

          count <- atomically (readTVar counter)
          count @?= 1

      , testCase "recovers from custom exceptions" $ do
          f <- mkFailN Custom1 2
          res <- try $ recovering'
            (constantDelay 5000 <> limitRetries 3)
            [const $ Handler $ \ Custom1 -> return shouldRetry]
            f
          (res :: Either Custom1 ()) @?= Right ()

      , testCase "fails beyond policy using custom exceptions" $ do
          f <- mkFailN Custom1 3
          res <- try $ recovering'
            (constantDelay 5000 <> limitRetries 2)
            [const $ Handler $ \ Custom1 -> return shouldRetry]
            f
          (res :: Either Custom1 ()) @?= Left Custom1

      , testCase "recoverAll won't catch exceptions which are not decendants of SomeException" $ do
          f <- mkFailN Custom1 4
          res <- try $ recoverAll
            (constantDelay 5000 <> limitRetries 3)
            f
          (res :: Either Custom1 ()) @?= Left Custom1

      , testCase "does not recover from unhandled exceptions" $ do
          f <- mkFailN Custom2 2
          res <- try $ recovering'
            (constantDelay 5000 <> limitRetries 5)
            [const $ Handler $ \ Custom1 -> return shouldRetry]
            f
          (res :: Either Custom2 ()) @?= Left Custom2


      , testCase "recovers in presence of multiple handlers" $ do
          f <- mkFailN Custom2 2
          res <- try $ recovering'
            (constantDelay 5000 <> limitRetries 5)
            [ const $ Handler $ \ Custom1 -> return shouldRetry
            , const $ Handler $ \ Custom2 -> return shouldRetry ]
            f
          (res :: Either Custom2 ()) @?= Right ()


      , testCase "general exceptions catch specific ones" $ do
          f <- mkFailN Custom2 2
          res <- try $ recovering'
            (constantDelay 5000 <> limitRetries 5)
            [ const $ Handler $ \ (_::SomeException) -> return shouldRetry ]
            f
          (res :: Either Custom2 ()) @?= Right ()


      , testCase "(redundant) even general catchers don't go beyond policy" $ do
          f <- mkFailN Custom2 3
          res <- try $ recovering'
            (constantDelay 5000 <> limitRetries 2)
            [ const $ Handler $ \ (_::SomeException) -> return shouldRetry ]
            f
          (res :: Either Custom2 ()) @?= Left Custom2


      , testCase "rethrows in presence of failed exception casts" $ do
          f <- mkFailN Custom2 3
          final <- try $ do
            res <- try $ recovering'
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
  [ testProperty "left identity" $ property $
      propIdentity (\p -> mempty <> p) id
  , testProperty "right identity" $ property $
      propIdentity (\p -> p <> mempty) id
  , testProperty "associativity" $ property $
      propAssociativity (\x y z -> x <> (y <> z)) (\x y z -> (x <> y) <> z)
  ]
  where
    propIdentity left right  = do
      retryStatus <- forAll genRetryStatus
      fixedDelay <- forAll (Gen.maybe (Gen.int (Range.linear 0 maxBound)))
      let calculateDelay _rs = fixedDelay
      let applyPolicy' f = getRetryPolicyM (f $ retryPolicy calculateDelay) retryStatus
          validRes = maybe True (>= 0)
      l <- liftIO $ applyPolicy' left
      r <- liftIO $ applyPolicy' right
      if validRes r && validRes l
        then l === r
        else return ()
    propAssociativity left right  = do
      retryStatus <- forAll genRetryStatus
      let genDelay = Gen.maybe (Gen.int (Range.linear 0 maxBound))
      delayA <- forAll genDelay
      delayB <- forAll genDelay
      delayC <- forAll genDelay
      let applyPolicy' f = liftIO $ getRetryPolicyM (f (retryPolicy (const delayA)) (retryPolicy (const delayB)) (retryPolicy (const delayC))) retryStatus
      res <- liftIO (liftA2 (==) (applyPolicy' left) (applyPolicy' right))
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
  [ testProperty "always produces positive delay with positive constants (no rollover)" $ property $ do
      delay <- forAll (Gen.int (Range.linear 0 maxBound))
      let res = runIdentity (simulatePolicy 1000 (exponentialBackoff delay))
          delays = catMaybes (snd <$> res)
          mnDelay = if null delays
                      then Nothing
                      else Just (minimum delays)
      case mnDelay of
        Nothing -> return ()
        Just n -> do
          footnote (show n ++ " is not >= 0")
          HH.assert (n >= 0)
  , testProperty "positive, nonzero exponential backoff is always incrementing" $ property $ do
     delay <- forAll (Gen.int (Range.linear 1 maxBound))
     let res = runIdentity (simulatePolicy 1000 (limitRetriesByDelay maxBound (exponentialBackoff delay)))
         delays = catMaybes (snd <$> res)
     sort delays === delays
     length (group delays) === length delays
  ]


-------------------------------------------------------------------------------
maskingStateTests :: TestTree
maskingStateTests = maskingStateTestsWith recovering


maskingStateTestsWith
    :: Monad m
    => (RetryPolicyM m -> [RetryStatus -> Handler IO Bool] -> (a -> IO b) -> IO ())
    -> TestTree
maskingStateTestsWith recovering' = testGroup "masking state"
  [ testCase "shouldn't change masking state in a recovered action" $ do
      maskingState <- EX.getMaskingState
      final <- try $ recovering' retryPolicyDefault testHandlers $ const $ do
        maskingState' <- EX.getMaskingState
        maskingState' @?= maskingState
        fail "Retrying..."
      assertBool
        ("Expected EX.IOException but didn't get one")
        (isLeft (final :: Either EX.IOException ()))

  , testCase "should mask asynchronous exceptions in exception handlers" $ do
      let checkMaskingStateHandlers =
            [ const $ Handler $ \(_ :: SomeException) -> do
                maskingState <- EX.getMaskingState
                maskingState @?= EX.MaskedInterruptible
                return shouldRetry
            ]
      final <- try $ recovering' retryPolicyDefault checkMaskingStateHandlers $ const $ fail "Retrying..."
      assertBool
        ("Expected EX.IOException but didn't get one")
        (isLeft (final :: Either EX.IOException ()))
  ]


-------------------------------------------------------------------------------
capDelayTests :: TestTree
capDelayTests = testGroup "capDelay"
  [ testProperty "respects limitRetries" $ property $ do
      retries <- forAll (Gen.int (Range.linear 1 100))
      cap <- forAll (Gen.int (Range.linear 1 maxBound))
      let policy = capDelay cap (limitRetries retries)
      let delays = runIdentity (simulatePolicy (retries + 1) policy)
      let Just lastDelay = lookup (retries - 1) delays
      let Just gaveUp = lookup retries delays
      let noDelay = 0
      lastDelay === Just noDelay
      gaveUp === Nothing
  , testProperty "does not allow any delays higher than the given delay" $ property $ do
      cap <- forAll (Gen.int (Range.linear 1 maxBound))
      baseDelay <- forAll (Gen.int (Range.linear 1 100))
      basePolicy <- forAllWith (const "RetryPolicy") (genScalingPolicy baseDelay)
      let policy = capDelay cap basePolicy
      let delays = catMaybes (snd <$> runIdentity (simulatePolicy 100 policy))
      let baddies = filter (> cap) delays
      baddies === []
  ]


-------------------------------------------------------------------------------
-- | Generates policies that increase on each iteration
genScalingPolicy :: (Alternative m) => Int -> m (RetryPolicyM Identity)
genScalingPolicy baseDelay =
  (pure (exponentialBackoff baseDelay) <|> pure (fibonacciBackoff baseDelay))


-------------------------------------------------------------------------------
limitRetriesByCumulativeDelayTests :: TestTree
limitRetriesByCumulativeDelayTests = testGroup "limitRetriesByCumulativeDelay"
  [ testProperty "never exceeds the given cumulative delay" $ property $ do
      baseDelay <- forAll (Gen.int (Range.linear 1 100))
      basePolicy <- forAllWith (const "RetryPolicy") (genScalingPolicy baseDelay)
      cumulativeDelayMax <- forAll (Gen.int (Range.linear 1 10000))
      let policy = limitRetriesByCumulativeDelay cumulativeDelayMax basePolicy
      let delays = catMaybes (snd <$> runIdentity (simulatePolicy 100 policy))
      footnoteShow delays
      let actualCumulativeDelay = sum delays
      footnote (show actualCumulativeDelay <> " <= " <> show cumulativeDelayMax)
      HH.assert (actualCumulativeDelay <= cumulativeDelayMax)

  ]

-------------------------------------------------------------------------------
quadraticDelayTests :: TestTree
quadraticDelayTests = quadraticDelayTestsWith recovering


quadraticDelayTestsWith
    :: Monad m
    => (RetryPolicyM m -> [RetryStatus -> Handler IO Bool] -> (a -> IO b) -> IO ())
    -> TestTree
quadraticDelayTestsWith recovering' = testGroup "quadratic delay"
  [ testProperty "recovering test with quadratic retry delay" $ property $ do
      startTime <- liftIO getCurrentTime
      timeout <- forAll (Gen.int (Range.linear 0 15))
      retries <- forAll (Gen.int (Range.linear 0 8))
      res <- liftIO $ try $ recovering'
        (exponentialBackoff timeout <> limitRetries retries)
        [const $ Handler (\(_::SomeException) -> return True)]
        (const $ throwM (userError "booo"))
      endTime <- liftIO getCurrentTime
      HH.assert (isLeftAnd isUserError res)
      let tmo = if retries > 0 then timeout * 2 ^ (retries - 1) else 0
      let ms' = ((fromInteger . toInteger $ tmo) / 1000000.0)
      HH.assert (diffUTCTime endTime startTime >= ms')
  ]


-------------------------------------------------------------------------------
overridingDelayTests :: TestTree
overridingDelayTests = testGroup "overriding delay"
  [ testGroup "actual delays don't exceed specified delays"
    [ testProperty "retryingDynamic" $
        testOverride
          retryingDynamic
          (\delays rs _ -> return $ ConsultPolicyOverrideDelay (delays !! rsIterNumber rs))
          (\_ _ -> liftIO getCurrentTime >>= \time -> tell [time])
    , testProperty "recoveringDynamic" $
        testOverride
          recoveringDynamic
          (\delays -> [\rs -> Handler (\(_::SomeException) -> return $ ConsultPolicyOverrideDelay (delays !! rsIterNumber rs))])
          (\delays rs -> do
              liftIO getCurrentTime >>= \time -> tell [time]
              if rsIterNumber rs < length delays
                then throwM (userError "booo")
                else return ()
          )
    ]
  ]
  where
    -- Transform a list of timestamps into a list of differences
    -- between adjacent timestamps.
    diffTimes = compareAdjacent (flip diffUTCTime)
    microsToNominalDiffTime = toNominal . picosecondsToDiffTime . (* 1000000) . fromIntegral
    toNominal :: DiffTime -> NominalDiffTime
    toNominal = realToFrac
    -- Generic test case used to test both "retryingDynamic" and "recoveringDynamic"
    testOverride retryer handler action = property $ do
      retryPolicy' <- forAll $ genPolicyNoLimit (Range.linear 1 1000000)
      delays <- forAll $ Gen.list (Range.linear 1 10) (Gen.int (Range.linear 10 1000))
      (_, measuredTimestamps) <- liftIO $ runWriterT $ retryer
        -- Stop retrying when we run out of delays
        (retryPolicy' <> limitRetries (length delays))
        (handler delays)
        (action delays)
      let expectedDelays = map microsToNominalDiffTime delays
      forM_ (zip (diffTimes measuredTimestamps) expectedDelays) $
        \(actual, expected) -> diff actual (>=) expected


-------------------------------------------------------------------------------
resumableTests :: TestTree
resumableTests = testGroup "resumable"
  [ testGroup "resumeRetrying"
      [ testCase "can resume" $ do
          retryingTest resumeRetrying (\_ _ -> pure shouldRetry)
      ]
  , testGroup "resumeRetryingDynamic"
      [ testCase "can resume" $ do
          retryingTest resumeRetryingDynamic (\_ _ -> pure $ ConsultPolicy)
      ]
  , testGroup "resumeRecovering"
      [ testCase "can resume" $ do
          recoveringTest resumeRecovering testHandlers
      ]
  , testGroup "resumeRecoveringDynamic"
      [ testCase "can resume" $ do
          recoveringTest resumeRecoveringDynamic testHandlersDynamic
      ]
  , testGroup "resumeRecoverAll"
      [ testCase "can resume" $ do
          recoveringTest
            (\status policy () action -> resumeRecoverAll status policy action)
            ()
      ]
  ]

retryingTest
  :: (RetryStatus -> RetryPolicyM IO -> p -> (RetryStatus -> IO ()) -> IO ())
  -> p
  -> IO ()
retryingTest resumableOp isRetryNeeded = do
  counterRef <- newIORef (0 :: Int)

  let go policy status = do
        atomicWriteIORef counterRef 0
        resumableOp
          status
          policy
          isRetryNeeded
          (const $ atomicModifyIORef' counterRef $ \n -> (1 + n, ()))

  let policy = limitRetries 2
  let nextStatus = nextStatusUsingPolicy policy

  go policy defaultRetryStatus
  (3 @=?) =<< readIORef counterRef

  go policy =<< nextStatus defaultRetryStatus
  (2 @=?) =<< readIORef counterRef

  go policy =<< nextStatus =<< nextStatus defaultRetryStatus
  (1 @=?) =<< readIORef counterRef

recoveringTest
  :: (RetryStatus -> RetryPolicyM IO -> handlers -> (RetryStatus -> IO ()) -> IO ())
  -> handlers
  -> IO ()
recoveringTest resumableOp handlers = do
  counterRef <- newIORef (0 :: Int)

  let go policy status = do
        action <- do
          mkFailUntilIO
            (\_ -> atomicModifyIORef' counterRef $ \n -> (1 + n, False))
            Custom1
        try $ resumableOp status policy handlers action

  let policy = limitRetries 2
  let nextStatus = nextStatusUsingPolicy policy

  do
    atomicWriteIORef counterRef 0
    res <- go policy defaultRetryStatus
    res @?= Left Custom1
    (3 @=?) =<< readIORef counterRef

  do
    atomicWriteIORef counterRef 0
    res <- go policy =<< nextStatus defaultRetryStatus
    res @?= Left Custom1
    (2 @=?) =<< readIORef counterRef

  do
    atomicWriteIORef counterRef 0
    res <- go policy =<< nextStatus =<< nextStatus defaultRetryStatus
    res @?= Left Custom1
    (1 @=?) =<< readIORef counterRef


-------------------------------------------------------------------------------
retryOnErrorTests :: TestTree
retryOnErrorTests = testGroup "retryOnError"
  [ testCase "passes in the error type" $ do
      errCalls <- newTVarIO []
      let policy = limitRetries 2
      let shouldWeRetry _retryStat e = do
            liftIO (atomically (modifyTVar' errCalls (++ [e])))
            return True
      let action rs = (throwError ("boom" ++ show (rsIterNumber rs)))
      res <- runExceptT (retryOnError policy shouldWeRetry action)
      res @?= (Left "boom2" :: Either String ())
      calls <- atomically (readTVar errCalls)
      calls @?= ["boom0", "boom1", "boom2"]
  ]

-------------------------------------------------------------------------------
nextStatusUsingPolicy :: RetryPolicyM IO -> RetryStatus -> IO RetryStatus
nextStatusUsingPolicy policy status = do
  applyPolicy policy status >>= \case
    Nothing -> do
      assertFailure "applying policy produced no new status"
    Just status' -> do
      pure status'


-------------------------------------------------------------------------------
isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False


-------------------------------------------------------------------------------
testHandlers :: [a -> Handler IO Bool]
testHandlers = [const $ Handler (\(_::SomeException) -> return shouldRetry)]


-------------------------------------------------------------------------------
testHandlersDynamic :: [a -> Handler IO RetryAction]
testHandlersDynamic =
  [const $ Handler (\(_::SomeException) -> return ConsultPolicy)]

-- | Apply a function to adjacent list items.
--
-- Ie.:
--    > compareAdjacent f [a0, a1, a2, a3, ..., a(n-2), a(n-1), an] =
--    >    [f a0 a1, f a1 a2, f a2 a3, ..., f a(n-2) a(n-1), f a(n-1) an]
--
-- Not defined for lists of length < 2.
compareAdjacent :: (a -> a -> b) -> [a] -> [b]
compareAdjacent f lst =
    reverse . snd $ foldl
      (\(a1, accum) a2 -> (a2, f a1 a2 : accum))
      (head lst, [])
      (tail lst)

data Custom1 = Custom1 deriving (Eq,Show,Read,Ord,Typeable)
data Custom2 = Custom2 deriving (Eq,Show,Read,Ord,Typeable)


instance Exception Custom1
instance Exception Custom2


-------------------------------------------------------------------------------
genRetryStatus :: MonadGen m => m RetryStatus
genRetryStatus = do
  n <- Gen.int (Range.linear 0 maxBound)
  d <- Gen.int (Range.linear 0 maxBound)
  l <- Gen.maybe (Gen.int (Range.linear 0 d))
  return $ defaultRetryStatus { rsIterNumber = n
                              , rsCumulativeDelay = d
                              , rsPreviousDelay = l}


-------------------------------------------------------------------------------
-- | Generate an arbitrary 'RetryPolicy' without any limits applied.
genPolicyNoLimit
    :: forall mg mr. (MonadGen mg, MIO.MonadIO mr)
    => Range Int
    -> mg (RetryPolicyM mr)
genPolicyNoLimit durationRange =
    Gen.choice
      [ genConstantDelay
      , genExponentialBackoff
      , genFullJitterBackoff
      , genFibonacciBackoff
      ]
  where
    genDuration = Gen.int durationRange
    -- Retry policies
    genConstantDelay = fmap constantDelay genDuration
    genExponentialBackoff = fmap exponentialBackoff genDuration
    genFullJitterBackoff = fmap fullJitterBackoff genDuration
    genFibonacciBackoff = fmap fibonacciBackoff genDuration

-- Needed to generate a 'RetryPolicyM' using 'forAll'
instance Show (RetryPolicyM m) where
    show = const "RetryPolicyM"


-------------------------------------------------------------------------------
-- | Create an action that will fail exactly N times with the given
-- exception and will then return () in any subsequent calls.
mkFailN :: (Exception e) => e -> Int -> IO (s -> IO ())
mkFailN e n = mkFailUntil (\iter -> iter >= n) e


-------------------------------------------------------------------------------
-- | Create an action that will fail with the given exception until the given
-- iteration predicate returns 'True', at which point the action will return
-- '()' in any subsequent calls.
mkFailUntil
    :: (Exception e)
    => (Int -> Bool)
    -> e
    -> IO (s -> IO ())
mkFailUntil p = mkFailUntilIO (pure . p)


-------------------------------------------------------------------------------
-- | The same as 'mkFailUntil' but allows doing IO in the predicate.
mkFailUntilIO
    :: (Exception e)
    => (Int -> IO Bool)
    -> e
    -> IO (s -> IO ())
mkFailUntilIO p e = do
    r <- newIORef 0
    return $ const $ do
      old <- atomicModifyIORef' r $ \ old -> (old+1, old)
      p old >>= \case
        True  -> return ()
        False -> throwM e


-------------------------------------------------------------------------------
gatherStatuses
    :: MonadIO m
    => RetryPolicyM (WriterT [RetryStatus] m)
    -> m [RetryStatus]
gatherStatuses policy = execWriterT $
  retrying policy (\_ _ -> return shouldRetry)
                  (\rs -> tell [rs])


-------------------------------------------------------------------------------
-- | Just makes things a bit easier to follow instead of a magic value
-- of @return True@
shouldRetry :: Bool
shouldRetry = True
