{-# LANGUAGE ScopedTypeVariables #-}

module QuadraticDelayRetrySpec where

import Control.Applicative
import Control.Retry
import Control.Monad.Catch
import Data.Time.Clock
import Data.Time.LocalTime ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import System.IO.Error

isLeftAnd :: (a -> Bool) -> Either a b -> Bool
isLeftAnd f ei = case ei of
  Left v -> f v
  _      -> False


{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = parallel $ describe "quadratic delay" $ do
  it "recovering test with quadratic retry delay"
     . property . monadicIO $ do
    startTime <- run getCurrentTime
    timeout <- (+2) . getSmall . getPositive <$> pick arbitrary
    retries <- pick . choose $ (0,10)
    res <- run . try $ recovering (RetrySettings (RLimit retries) True timeout)
                              [Handler (\(_::SomeException) -> return True)]
                              (throwM (userError "booo"))
    endTime <- run getCurrentTime
    QCM.assert (isLeftAnd isUserError res)
    let tmo = if retries > 0 then timeout * 2 ^ (retries - 1) else 0
    let ms' = ((fromInteger . toInteger $ tmo) / 1000.0)
    QCM.assert (diffUTCTime endTime startTime >= ms')

