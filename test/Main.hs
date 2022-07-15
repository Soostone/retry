module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Tests.Control.Retry
import qualified Tests.UnliftIO.Retry
-------------------------------------------------------------------------------



main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "retry"
  [ Tests.Control.Retry.tests
  , Tests.UnliftIO.Retry.tests
  ]
