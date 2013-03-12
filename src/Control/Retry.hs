{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Retry
-- Copyright   :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman
-- Stability   :  provisional
--
-- This module exposes combinators that can wrap arbitrary monadic
-- actions. They run the action and potentially retry running it with
-- some configurable delay for a configurable number of times.
--
-- The express purpose of this library is to make it easier to work
-- with IO and especially network IO actions that often experience
-- temporary failure that warrant retrying of the original action. For
-- example, a database query may time out for a while, in which case
-- we should delay a bit and retry the query.
----------------------------------------------------------------------------


module Control.Retry
    (
      -- * High Level Operation
      RetrySettings (..)
    , retrying
    , recovering
    , recoverAll

    -- * Utilities
    , delay
    , flatDelay
    , backoffDelay
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception     (SomeException)
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.Default
import           Prelude               hiding (catch)
-------------------------------------------------------------------------------


-- | Settings for retry behavior. Simply using 'def' for default
-- values should work in most cases.
data RetrySettings = RetrySettings {
      numRetries :: Int
    -- ^ Number of retries. Defaults to 5.
    , backoff    :: Bool
    -- ^ Whether to implement exponential backoff in retries. Defaults
    -- to True.
    , baseDelay  :: Int
    -- ^ The base delay in miliseconds. Defaults to 50. Without
    -- 'backoff', this is the delay. With 'backoff', this base delay
    -- will grow by a factor of 2 on each subsequent retry.
    }


instance Default RetrySettings where
    def = RetrySettings 5 True 50


-- | Delay thread using backoff delay for the nth retry.
backoffDelay :: (Integral b, MonadIO m) => RetrySettings -> b -> m ()
backoffDelay set@RetrySettings{..} !n = liftIO (threadDelay (2^n * delay set))

-- | Delay thread using flat delay
flatDelay :: MonadIO m => RetrySettings -> t -> m ()
flatDelay set@RetrySettings{..} !n = liftIO (threadDelay $ delay set)

-- | Delay in micro seconds
delay :: RetrySettings -> Int
delay RetrySettings{..} = baseDelay * 1000


-- | Retry combinator for actions that don't raise exceptions, but
-- signal in their type the outcome has failed. Examples are the
-- 'Maybe', 'Either' and 'EitherT' monads.
--
-- Let's write a function that always fails and watch this combinator
-- retry it 5 additional times following the initial run:
--
-- >>> import Data.Maybe
-- >>> let f = putStrLn "Running action" >> return Nothing
-- >>> retrying def isNothing f
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Nothing
--
-- Note how the latest failing result is returned after all retries
-- have been exhausted.
retrying :: MonadIO m
         => RetrySettings
         -> (b -> Bool)
         -- ^ A function to check whether the result should be
         -- retried. If True, we delay and retry the operation.
         -> m b
         -- ^ Action to run
         -> m b
retrying set@RetrySettings{..} chk f = go 0
    where
      go n = do
          res <- f
          case chk res of
            True ->
              case n >= numRetries of
                True -> return res
                False -> do
                    if backoff
                      then backoffDelay set n
                      else flatDelay set n
                    go $! n+1
            False -> return res



-- | Retry ALL exceptions that may be raised. To be used with caution;
-- this matches the exception on 'SomeException'.
--
-- See how the action below is run once and retried 5 more times
-- before finally failing for good:
--
-- >>> let f = putStrLn "Running action" >> error "this is an error"
-- >>> recoverAll def f
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- *** Exception: this is an error
recoverAll :: MonadCatchIO m
         => RetrySettings
         -> m a
         -> m a
recoverAll set f = recovering set [h] f
    where
      h = Handler $ \ (e :: SomeException) -> return True


-- | Run an action and recover from a raised exception by potentially
-- retrying the action a number of times.
recovering :: forall m a. MonadCatchIO m
           => RetrySettings
           -- ^ Just use 'def' faor default settings
           -> [Handler m Bool]
           -- ^ Should a given exception be retried? Action will be
           -- retried if this returns True.
           -> m a
           -- ^ Action to perform
           -> m a
recovering set@RetrySettings{..} hs f = go 0
    where
      -- | Convert a (e -> m Bool) handler into (e -> m a) so it can
      -- be wired into the 'catches' combinator.
      transHandler :: Int -> Handler m Bool -> Handler m a
      transHandler n (Handler h) = Handler $ \ e -> do
          chk <- h e
          case chk of
            True ->
              case n >= numRetries of
                True -> throw e
                False -> do
                    if backoff
                      then backoffDelay set n
                      else flatDelay set n
                    go $! n+1
            False -> throw e

      -- handle :: forall e. Exception e => Handler m Bool -> Int -> e -> m a
      -- handle (Handler h) n e = do
      --     chk <- h e
      --     case chk of
      --       True ->
      --         case n >= numRetries of
      --           True -> throw e
      --           False -> if backoff then backoffRetry n else flatRetry n
      --       False -> throw e

      go n = f `catches` map (transHandler n) hs



                              ------------------
                              -- Simple Tests --
                              ------------------



-- data TestException = TestException deriving (Show, Typeable)
-- data AnotherException = AnotherException deriving (Show, Typeable)

-- instance Exception TestException
-- instance Exception AnotherException


-- test = retrying def [h1,h2] f
--     where
--       f = putStrLn "Running action" >> throw AnotherException
--       h1 = Handler $ \ (e :: TestException) -> return False
--       h2 = Handler $ \ (e :: AnotherException) -> return True


