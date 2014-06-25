{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
    , RetryLimit(..)
    , limitedRetries
    , unlimitedRetries

    , retrying
    , recovering
    , recoverAll

    -- * Backoff strategies
    , constantBackoff
    , exponentialBackoff
    , fibonacciBackoff

    -- * Utilities
    , delay
    , performDelay
    ) where

-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Default.Class
import           Prelude                hiding (catch)
-------------------------------------------------------------------------------


data RetryLimit = RLimit Int
                | RNoLimit


-- | Set a limited number of retries. Default in 'def' is 5.
limitedRetries :: Int -> RetryLimit
limitedRetries = RLimit


-- | Set an unlimited number of retries. Note that with this option
-- turned on, the combinator will keep retrying the action
-- indefinitely and might essentially hang in some cases.
unlimitedRetries :: RetryLimit
unlimitedRetries = RNoLimit


-- | Settings for retry behavior. Simply using 'def' for default
-- values should work in most cases.
data RetrySettings = RetrySettings {
      numRetries :: RetryLimit
    -- ^ Number of retries. Defaults to 5.
    , backoff    :: Int -> Int -> Int
    -- ^ Backoff strategy. It takes a base delay and an iteration number
    -- starting at 0. Defaults to 'exponentialBackoff'.
    , baseDelay  :: Int
    -- ^ The base delay in miliseconds. Defaults to 50. Without
    -- 'backoff', this is the delay. With 'backoff', this base delay
    -- will grow by a factor of 2 on each subsequent retry.
    }


instance Default RetrySettings where
    def = RetrySettings (limitedRetries 5) exponentialBackoff 50

-- | Delay for nth iteration of constant backoff, in microseconds
constantBackoff
    :: Int
    -- ^ Base delay in microseconds
    -> Int
    -- ^ Iteration number, starting at 0. This number doesn't affect the delay.
    -> Int
    -- ^ Delay in microseconds
constantBackoff = const

-- | Delay for nth iteration of exponential backoff, in microseconds
exponentialBackoff
    :: Int
    -- ^ Base delay in microseconds
    -> Int
    -- ^ Iteration number, starting at 0.
    -> Int
    -- ^ Delay in microseconds
exponentialBackoff base n = 2^n * base

-- | Delay for nth iteration of fibonacci backoff, in microseconds
fibonacciBackoff
    :: Int
    -- ^ Base delay in microseconds
    -> Int
    -- ^ Iteration number, starting at 0.
    -> Int
    -- ^ Delay in microseconds
fibonacciBackoff base n = fib (n + 1) (0, base)
    where
      fib 0 (a, _) = a
      fib !m (!a, !b) = fib (m-1) (b, a + b)

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
      retry n = do
          performDelay set n
          go $! n+1

      go n = do
          res <- f
          case chk res of
            True ->
              case numRetries of
                RNoLimit -> retry n
                RLimit lim -> if n >= lim then return res else retry n
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
recoverAll :: (MonadIO m, MonadCatch m)
         => RetrySettings
         -> m a
         -> m a
recoverAll set f = recovering set [h] f
    where
      h = Handler $ \ (_ :: SomeException) -> return True


-- | Perform 'threadDelay' for the nth retry for the given settings.
performDelay :: MonadIO m => RetrySettings -> Int -> m ()
performDelay set@RetrySettings{..} =
    liftIO . threadDelay . backoff (delay set)

-- | Run an action and recover from a raised exception by potentially
-- retrying the action a number of times.
recovering :: forall m a. (MonadIO m, MonadCatch m)
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
      retry n = do
          performDelay set n
          go $! n+1


      -- | Convert a (e -> m Bool) handler into (e -> m a) so it can
      -- be wired into the 'catches' combinator.
      transHandler :: Int -> Handler m Bool -> Handler m a
      transHandler n (Handler h) = Handler $ \ e -> do
          chk <- h e
          case chk of
            True ->
              case numRetries of
                RNoLimit -> retry n
                RLimit lim -> if n >= lim then throwM e else retry n
            False -> throwM e

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
