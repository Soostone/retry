{-# LANGUAGE ScopedTypeVariables #-}

module RetrySpec where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception (MaskingState(..), getMaskingState)
import           Control.Monad.Catch
import           Control.Retry
import           Data.Monoid
import           Data.Default.Class (def)
import           Data.Time.Clock
import           Data.Time.LocalTime      ()
import           System.IO.Error
import           Test.HUnit (Test(TestCase), (@?=))
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Monadic  as QCM
-------------------------------------------------------------------------------


isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False

testHandlers :: [Int -> Handler IO Bool]
testHandlers = [const $ Handler (\(_::SomeException) -> return True)]

{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = parallel $ describe "retry" $ do

  it "recovering test without quadratic retry delay"
     . property . monadicIO $ do
    startTime <- run getCurrentTime
    timeout <- pick . choose $ (0,15)
    retries <- getSmall . getPositive <$> pick arbitrary
    res <- run . try $ recovering (constantDelay timeout <> limitRetries retries)
                              testHandlers
                              (throwM (userError "booo"))
    endTime <- run getCurrentTime
    QCM.assert (isLeftAnd isUserError res)
    let ms' = (fromInteger . toInteger $ (timeout * retries)) / 1000000.0
    QCM.assert (diffUTCTime endTime startTime >= ms')

  describe "Policy is a monoid" $ do
    let toPolicy = RetryPolicy . apply
    let prop left right  =
          property $ \a x ->
            let applyPolicy f = getRetryPolicy (f $ toPolicy a) x
                l = applyPolicy left
                r = applyPolicy right
                validRes = maybe True (>= 0)
            in  (validRes r && validRes l) ==> l == r
    let prop3 left right  =
          property $ \a b c x ->
            let applyPolicy f = getRetryPolicy (f (toPolicy a) (toPolicy b) (toPolicy c)) x
            in applyPolicy left == applyPolicy right

    it "left identity" $
      prop (\p -> mempty <> p) id

    it "right identity" $
      prop (\p -> p <> mempty) id

    it "associativity" $
      prop3 (\x y z -> x <> (y <> z)) (\x y z -> (x <> y) <> z)

  it "shouldn't change masking state in a recovered action" $ TestCase $ do
    maskingState <- getMaskingState
    shouldThrow
      (recovering def testHandlers $ do
        maskingState' <- getMaskingState
        maskingState' @?= maskingState
        fail "Retrying...")
      anyIOException

  it "should mask asynchronous exceptions in exception handlers" $ TestCase $ do
    let checkMaskingStateHandlers =
          [ const $ Handler $ \(_ :: SomeException) -> do
              maskingState <- getMaskingState
              maskingState @?= MaskedInterruptible
              return True
          ]
    shouldThrow
      (recovering def checkMaskingStateHandlers $ fail "Retrying...")
      anyIOException
