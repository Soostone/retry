{-# LANGUAGE RankNTypes            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  UnliftIO.Retry
-- Copyright   :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- License     :  BSD3
--
-- Maintainer  :  Patrick Brisbin <pbrisbin@gmail.com>
-- Stability   :  provisional
--
-- Unlifted "Control.Retry".
--
-- @since 0.9.3.0
----------------------------------------------------------------------------


module UnliftIO.Retry
    (
      -- * Types and Operations
      RetryPolicyM (..)
    , RetryPolicy
    , retryPolicy
    , retryPolicyDefault
    , natTransformRetryPolicy
    , RetryAction (..)
    , toRetryAction
    , RetryStatus (..)
    , defaultRetryStatus
    , applyPolicy
    , applyAndDelay


    -- ** Lenses for 'RetryStatus'
    , rsIterNumberL
    , rsCumulativeDelayL
    , rsPreviousDelayL

    -- * Applying Retry Policies
    , retrying
    , retryingDynamic
    , recovering
    , recoveringDynamic
    , stepping
    , recoverAll
    , skipAsyncExceptions
    , logRetries
    , defaultLogMsg
    , retryOnError
    -- ** Resumable variants
    , resumeRetrying
    , resumeRetryingDynamic
    , resumeRecovering
    , resumeRecoveringDynamic
    , resumeRecoverAll

    -- * Retry Policies
    , constantDelay
    , exponentialBackoff
    , fullJitterBackoff
    , fibonacciBackoff
    , limitRetries

    -- * Policy Transformers
    , limitRetriesByDelay
    , limitRetriesByCumulativeDelay
    , capDelay

    -- * Development Helpers
    , simulatePolicy
    , simulatePolicyPP
    ) where

-------------------------------------------------------------------------------
import           Control.Retry hiding
    ( recoverAll
    , recovering
    , recoveringDynamic
    , resumeRecovering
    , resumeRecoveringDynamic
    , resumeRecoverAll
    , stepping
    )
import qualified Control.Retry as Retry
import           Control.Monad.Catch (Handler(..))
import           Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import           Prelude
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Run an action and recover from a raised exception by potentially
-- retrying the action a number of times. Note that if you're going to
-- use a handler for 'SomeException', you should add explicit cases
-- *earlier* in the list of handlers to reject 'AsyncException' and
-- 'SomeAsyncException', as catching these can cause thread and
-- program hangs. 'recoverAll' already does this for you so if you
-- just plan on catching 'SomeException', you may as well use
-- 'recoverAll'
recovering
    :: MonadUnliftIO m
    => RetryPolicyM m
    -- ^ Just use 'retryPolicyDefault' for default settings
    -> [RetryStatus -> Handler m Bool]
    -- ^ Should a given exception be retried? Action will be
    -- retried if this returns True *and* the policy allows it.
    -- This action will be consulted first even if the policy
    -- later blocks it.
    -> (RetryStatus -> m a)
    -- ^ Action to perform
    -> m a
recovering = resumeRecovering defaultRetryStatus


-------------------------------------------------------------------------------
-- | A variant of 'recovering' that allows specifying the initial
-- 'RetryStatus' so that a recovering operation may pick up where it left
-- off in regards to its retry policy.
resumeRecovering
    :: MonadUnliftIO m
    => RetryStatus
    -> RetryPolicyM m
    -- ^ Just use 'retryPolicyDefault' for default settings
    -> [RetryStatus -> Handler m Bool]
    -- ^ Should a given exception be retried? Action will be
    -- retried if this returns True *and* the policy allows it.
    -- This action will be consulted first even if the policy
    -- later blocks it.
    -> (RetryStatus -> m a)
    -- ^ Action to perform
    -> m a
resumeRecovering retryStatus policy hs f = withRunInIO $ \runInIO ->
    Retry.resumeRecovering
        retryStatus
        (transRetryPolicy runInIO policy)
        (map ((.) $ transHandler runInIO) hs)
        (runInIO . f)


-------------------------------------------------------------------------------
-- | The difference between this and 'recovering' is the same as
--  the difference between 'retryingDynamic' and 'retrying'.
recoveringDynamic
    :: MonadUnliftIO m
    => RetryPolicyM m
    -- ^ Just use 'retryPolicyDefault' for default settings
    -> [RetryStatus -> Handler m RetryAction]
    -- ^ Should a given exception be retried? Action will be
    -- retried if this returns either 'ConsultPolicy' or
    -- 'ConsultPolicyOverrideDelay' *and* the policy allows it.
    -- This action will be consulted first even if the policy
    -- later blocks it.
    -> (RetryStatus -> m a)
    -- ^ Action to perform
    -> m a
recoveringDynamic = resumeRecoveringDynamic defaultRetryStatus


-------------------------------------------------------------------------------
-- | A variant of 'recoveringDynamic' that allows specifying the initial
-- 'RetryStatus' so that a recovering operation may pick up where it left
-- off in regards to its retry policy.
resumeRecoveringDynamic
    :: MonadUnliftIO m
    => RetryStatus
    -> RetryPolicyM m
    -- ^ Just use 'retryPolicyDefault' for default settings
    -> [RetryStatus -> Handler m RetryAction]
    -- ^ Should a given exception be retried? Action will be
    -- retried if this returns either 'ConsultPolicy' or
    -- 'ConsultPolicyOverrideDelay' *and* the policy allows it.
    -- This action will be consulted first even if the policy
    -- later blocks it.
    -> (RetryStatus -> m a)
    -- ^ Action to perform
    -> m a
resumeRecoveringDynamic retryStatus policy hs f = withRunInIO $ \runInIO ->
    Retry.resumeRecoveringDynamic
        retryStatus
        (transRetryPolicy runInIO policy)
        (map ((.) $ transHandler runInIO) hs)
        (runInIO . f)


-------------------------------------------------------------------------------
-- | Retry ALL exceptions that may be raised. To be used with caution;
-- this matches the exception on 'SomeException'. Note that this
-- handler explicitly does not handle 'AsyncException' nor
-- 'SomeAsyncException' (for versions of base >= 4.7). It is not a
-- good idea to catch async exceptions as it can result in hanging
-- threads and programs. Note that if you just throw an exception to
-- this thread that does not descend from SomeException, recoverAll
-- will not catch it.
--
-- See how the action below is run once and retried 5 more times
-- before finally failing for good:
--
-- >>> let f _ = putStrLn "Running action" >> error "this is an error"
-- >>> recoverAll retryPolicyDefault f
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- Running action
-- *** Exception: this is an error
recoverAll
     :: MonadUnliftIO m
     => RetryPolicyM m
     -> (RetryStatus -> m a)
     -> m a
recoverAll = resumeRecoverAll defaultRetryStatus


-------------------------------------------------------------------------------
-- | A variant of 'recoverAll' that allows specifying the initial
-- 'RetryStatus' so that a recovering operation may pick up where it left
-- off in regards to its retry policy.
resumeRecoverAll
     :: MonadUnliftIO m
     => RetryStatus
     -> RetryPolicyM m
     -> (RetryStatus -> m a)
     -> m a
resumeRecoverAll retryStatus policy f = withRunInIO $ \runInIO ->
    Retry.resumeRecoverAll
        retryStatus
        (transRetryPolicy runInIO policy)
        (runInIO . f)

-------------------------------------------------------------------------------
-- | A version of 'recovering' that tries to run the action only a
-- single time. The control will return immediately upon both success
-- and failure. Useful for implementing retry logic in distributed
-- queues and similar external-interfacing systems.
stepping
    :: MonadUnliftIO m
    => RetryPolicyM m
    -- ^ Just use 'retryPolicyDefault' for default settings
    -> [RetryStatus -> Handler m Bool]
    -- ^ Should a given exception be retried? Action will be
    -- retried if this returns True *and* the policy allows it.
    -- This action will be consulted first even if the policy
    -- later blocks it.
    -> (RetryStatus -> m ())
    -- ^ Action to run with updated status upon failure.
    -> (RetryStatus -> m a)
    -- ^ Main action to perform with current status.
    -> RetryStatus
    -- ^ Current status of this step
    -> m (Maybe a)
stepping policy hs schedule f s = withRunInIO $ \runInIO ->
    Retry.stepping
        (transRetryPolicy runInIO policy)
        (map ((.) $ transHandler runInIO) hs)
        (runInIO . schedule)
        (runInIO . f)
        s


-------------------------------------------------------------------------------
transRetryPolicy :: (forall a. m a -> n a) -> RetryPolicyM m -> RetryPolicyM n
transRetryPolicy f (RetryPolicyM p) = RetryPolicyM $ f . p


-------------------------------------------------------------------------------
transHandler :: (forall b. m b -> n b) -> Handler m a -> Handler n a
transHandler f (Handler h) = Handler $ f . h
