{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RetrySpec where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception        (MaskingState (..), getMaskingState)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Default.Class       (def)
import           Data.IORef
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.LocalTime      ()
import           Data.Typeable
import           System.IO.Error
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit               (Test (TestCase), (@?=))
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Monadic  as QCM
-------------------------------------------------------------------------------
import           Control.Retry
-------------------------------------------------------------------------------


isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False

testHandlers :: [a -> Handler IO Bool]
testHandlers = [const $ Handler (\(_::SomeException) -> return True)]


data Custom1 = Custom1 deriving (Eq,Show,Read,Ord,Typeable)
data Custom2 = Custom2 deriving (Eq,Show,Read,Ord,Typeable)


instance Exception Custom1
instance Exception Custom2


instance Arbitrary RetryStatus where
  arbitrary = do
    Positive n <- arbitrary
    Positive d <- arbitrary
    l <- arbitrary `suchThatMaybe` \(Positive n) -> n <= d
    return (defaultRetryStatus { rsRetryNumber = n
                               , rsCumulativeDelay = d
                               , rsPreviousDelay = getPositive <$> l})

instance CoArbitrary RetryStatus

instance Function RetryStatus where
  function = functionMap (\rs -> (rsRetryNumber rs, rsCumulativeDelay rs, rsPreviousDelay rs))
                         (\(n, d, l) -> defaultRetryStatus { rsRetryNumber = n
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
        True -> return ()
        False -> throwM e



{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = parallel $ describe "retry" $ do

  it "recovering test without quadratic retry delay"
     . property . monadicIO $ do
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


  describe "recovering - exception hierarcy semantics" $ do

    it "recovers from custom exceptions" $ do
      f <- mkFailN Custom1 2
      res <- try $ recovering
        (constantDelay 5000 <> limitRetries 3)
        [const $ Handler $ \ Custom1 -> return True]
        f
      (res :: Either Custom1 ()) `shouldBe` Right ()


    it "fails beyond policy using custom exceptions" $ do
      f <- mkFailN Custom1 3
      res <- try $ recovering
        (constantDelay 5000 <> limitRetries 2)
        [const $ Handler $ \ Custom1 -> return True]
        f
      (res :: Either Custom1 ()) `shouldBe` Left Custom1


    it "does not recover from unhandled exceptions" $ do
      f <- mkFailN Custom2 2
      res <- try $ recovering
        (constantDelay 5000 <> limitRetries 5)
        [const $ Handler $ \ Custom1 -> return True]
        f
      (res :: Either Custom2 ()) `shouldBe` Left Custom2


    it "recovers in presence of multiple handlers" $ do
      f <- mkFailN Custom2 2
      res <- try $ recovering
        (constantDelay 5000 <> limitRetries 5)
        [ const $ Handler $ \ Custom1 -> return True
        , const $ Handler $ \ Custom2 -> return True ]
        f
      (res :: Either Custom2 ()) `shouldBe` Right ()


    it "general exceptions catch specific ones" $ do
      f <- mkFailN Custom2 2
      res <- try $ recovering
        (constantDelay 5000 <> limitRetries 5)
        [ const $ Handler $ \ (_::SomeException) -> return True ]
        f
      (res :: Either Custom2 ()) `shouldBe` Right ()


    it "(redundant) even general catchers don't go beyond policy" $ do
      f <- mkFailN Custom2 3
      res <- try $ recovering
        (constantDelay 5000 <> limitRetries 2)
        [ const $ Handler $ \ (_::SomeException) -> return True ]
        f
      (res :: Either Custom2 ()) `shouldBe` Left Custom2


    it "works as expected in presence of failed exception casts" $ do
      f <- mkFailN Custom2 3
      flip shouldThrow anyException $ do
        res <- try $ recovering
          (constantDelay 5000 <> limitRetries 2)
          [ const $ Handler $ \ (_::SomeException) -> return True ]
          f
        (res :: Either Custom1 ()) `shouldBe` Left Custom1



  describe "Policy is a monoid" $ do
    let toPolicy = retryPolicy . apply
    let prop left right  =
          property $ \a x ->
            let applyPolicy f = getRetryPolicyM (f $ toPolicy a) x
                validRes = maybe True (>= 0)
            in  monadicIO $ do
                l <- liftIO $ applyPolicy left
                r <- liftIO $ applyPolicy right
                if validRes r && validRes l
                  then assert (l == r)
                  else return ()

    let prop3 left right  =
          property $ \a b c x ->
            let applyPolicy f = liftIO $ getRetryPolicyM (f (toPolicy a) (toPolicy b) (toPolicy c)) x
            in monadicIO $ do
                  res <- (==) <$> applyPolicy left <*> applyPolicy right
                  assert res

    it "left identity" $
      prop (\p -> mempty <> p) id

    it "right identity" $
      prop (\p -> p <> mempty) id

    it "associativity" $
      prop3 (\x y z -> x <> (y <> z)) (\x y z -> (x <> y) <> z)

  describe "retry status" $ do

    it "passes in the correct retry status each time" $ do
      let policy = limitRetries 2 <> constantDelay 100
      r <- newIORef []
      retrying policy (\_ _ -> return True)
                      (\rs -> modifyIORef' r (\acc -> acc ++ [rs]))
      rses <- readIORef r
      rsRetryNumber <$> rses @?= [0, 1, 2]
      rsCumulativeDelay <$> rses @?= [0, 100, 200]
      rsPreviousDelay <$> rses @?= [Nothing, Just 0, Just 100]

  describe "masking state" $ do

    it "shouldn't change masking state in a recovered action" $ do
      maskingState <- getMaskingState
      shouldThrow
        (recovering def testHandlers $ const $ do
          maskingState' <- getMaskingState
          maskingState' @?= maskingState
          fail "Retrying...")
        anyIOException

    it "should mask asynchronous exceptions in exception handlers" $ do
      let checkMaskingStateHandlers =
            [ const $ Handler $ \(_ :: SomeException) -> do
                maskingState <- getMaskingState
                maskingState @?= MaskedInterruptible
                return True
            ]
      shouldThrow
        (recovering def checkMaskingStateHandlers $ const $ fail "Retrying...")
        anyIOException
