module Tests.UnliftIO.Retry
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.HUnit            (testCase)
-------------------------------------------------------------------------------
import           UnliftIO.Retry
import           Tests.Control.Retry hiding (tests)
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "UnliftIO.Retry"
  [ recoveringTests
  , maskingStateTests
  , quadraticDelayTests
  , resumableTests
  ]


-------------------------------------------------------------------------------
recoveringTests :: TestTree
recoveringTests = recoveringTestsWith recovering


-------------------------------------------------------------------------------
maskingStateTests :: TestTree
maskingStateTests = maskingStateTestsWith recovering


-------------------------------------------------------------------------------
quadraticDelayTests :: TestTree
quadraticDelayTests = quadraticDelayTestsWith recovering


-------------------------------------------------------------------------------
resumableTests :: TestTree
resumableTests = testGroup "resumable"
  [ testGroup "resumeRecovering"
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
