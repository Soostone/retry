module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Tests.Control.Retry
-------------------------------------------------------------------------------



main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "retry"
  [ Tests.Control.Retry.tests
  ]
