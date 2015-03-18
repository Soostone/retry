{-# LANGUAGE ScopedTypeVariables #-}

module QuadraticDelayRetrySpec where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Retry
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.LocalTime     ()
import           System.IO.Error
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic as QCM

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
    timeout <- pick . choose $ (0,15)
    retries <- pick . choose $ (0,8)
    res <- run . try $ recovering (exponentialBackoff timeout <> limitRetries retries)
                              [const $ Handler (\(_::SomeException) -> return True)]
                              (throwM (userError "booo"))
    endTime <- run getCurrentTime
    QCM.assert (isLeftAnd isUserError res)
    let tmo = if retries > 0 then timeout * 2 ^ (retries - 1) else 0
    let ms' = ((fromInteger . toInteger $ tmo) / 1000000.0)
    QCM.assert (diffUTCTime endTime startTime >= ms')

