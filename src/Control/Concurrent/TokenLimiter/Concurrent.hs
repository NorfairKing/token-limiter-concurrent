{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Control.Concurrent.TokenLimiter.Concurrent
  ( Count, getCount, CountType(..),
    TokenLimitConfig (..),
    MonotonicTime, MonotonicTimeType(..),
    TokenLimiter (..),
    makeTokenLimiter,
    canDebit,
    tryDebit,
    waitDebit,

    -- * Helper functions
    computeCurrentCount,
  )
where

import Control.Concurrent
import Data.Word
import qualified GHC.Clock as Clock
import GHC.Generics (Generic)
import Numeric.Natural


-- | Make the Count type semantic with it's usage
data CountType = Initial | Max | PerSecond | LastServiced | Current | Debit

-- | An amount of tokens
newtype Count (ct :: CountType) = Count { getCount :: Word64 }
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Generic)

-- | A configuration for 'TokenLimiter'
data TokenLimitConfig = TokenLimitConfig
  { -- | How many tokens should be in the bucket when it's created
    tokenLimitConfigInitialTokens :: !(Count 'Initial),
    -- | Maximum number of tokens the bucket can hold at any one time
    tokenLimitConfigMaxTokens :: !(Count 'Max),
    -- | How many tokens are added to the bucket per second
    tokenLimitConfigTokensPerSecond :: !(Count 'PerSecond)
  }
  deriving (Show, Eq, Generic)

-- | Make MonotonicTime semantic with it's usage
data MonotonicTimeType = LastServicedTime | Now

-- | A type synonym for a number of "monotonic time" nanoseconds.
--
-- This only exists because it is also a 'Word64' and would be too easy to confuse with a 'Count'.
newtype MonotonicTime (m :: MonotonicTimeType) = MonotonicTime { getMonotonicTime :: Word64 }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic)

-- | A token bucket-based rate limiter
--
-- This token limiter is thread-safe and guarantees that:
--
-- * <https://en.wikipedia.org/wiki/Thundering_herd_problem There will be no thundering herd problem>
-- * <https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Concurrent-MVar.html#v:modifyMVar Fairness: Waiting processes will be serviced in a first-come first-service order.>
data TokenLimiter = TokenLimiter
  { tokenLimiterConfig :: !TokenLimitConfig,
    -- | The last time the limiter was used, and what the token count was at that time.
    --
    -- Not that this library assumes that you never put anything into this mvar
    -- yourself and only use the functions in this library to interact with it.
    tokenLimiterLastServiced :: !(MVar (MonotonicTime 'LastServicedTime, Count 'LastServiced))
  }
  deriving (Eq, Generic)


data EnoughAvailable = Available | Exhausted


getLastServiced :: Count 'Initial -> Count 'Max -> Count 'LastServiced
getLastServiced (Count initial) (Count max') = Count $ initial `min` max'


lastServicedIsNow :: MonotonicTime 'Now -> MonotonicTime 'LastServicedTime
lastServicedIsNow (MonotonicTime now) = MonotonicTime now


getMonotonicTimeNSec :: IO (MonotonicTime 'Now)
getMonotonicTimeNSec = MonotonicTime <$> Clock.getMonotonicTimeNSec


-- | Make a token limiter
--
-- The initial number of tokens will be the minimum of the 'tokenLimitConfigInitialTokens' and the 'tokenLimitConfigMaxTokens',
makeTokenLimiter :: TokenLimitConfig -> IO TokenLimiter
makeTokenLimiter tokenLimiterConfig = do
  MonotonicTime now <- getMonotonicTimeNSec
  tokenLimiterLastServiced <- newMVar (MonotonicTime now, getLastServiced (tokenLimitConfigInitialTokens tokenLimiterConfig) (tokenLimitConfigMaxTokens tokenLimiterConfig))
  pure TokenLimiter {..}


enoughAvailable :: Count 'Current -> Count 'Debit -> EnoughAvailable
enoughAvailable (Count currentCount) (Count debit) = if currentCount >= debit then Available else Exhausted


getNewCount :: Count 'Current -> Count 'Debit -> Count 'LastServiced
getNewCount (Count currentCount) (Count debit) = Count $ currentCount - debit


-- | Ask if we could debit a number of tokens, without actually doing it.
--
-- Note that this information can become stale _very_ quickly.
-- If you want to also actually debit a number of tokens, use 'tryDebit' instead.
canDebit :: TokenLimiter -> Count 'Debit -> IO EnoughAvailable
canDebit TokenLimiter {..} debit = withMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  pure $ enoughAvailable currentCount debit

-- | Check if we can debit a number of tokens, and do it if possible.
--
-- The returned boolean represents whether the tokens were debited.
tryDebit :: TokenLimiter -> Count 'Debit -> IO EnoughAvailable
tryDebit TokenLimiter {..} debit = modifyMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  pure $ case enoughAvailable currentCount debit of
    Available -> ((lastServicedIsNow now, getNewCount currentCount debit), Available)
    Exhausted -> ((lastServiced, countThen), Exhausted)

-- | Wait until the given number of tokens can be debited
waitDebit :: TokenLimiter -> Count 'Debit -> IO ()
waitDebit TokenLimiter {..} debit = modifyMVar_ tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
      extraTokensNeeded (Count debit) (Count currentCount) = Count $ debit - currentCount
  case enoughAvailable currentCount debit of
    Available -> pure (lastServicedIsNow now, getNewCount currentCount debit)
    Exhausted -> do
      let microsecondsToWaitDouble :: Double
          microsecondsToWaitDouble =
            1_000_000
              * fromIntegral (extraTokensNeeded debit currentCount :: Count 'PerSecond)
              / fromIntegral (tokenLimitConfigTokensPerSecond tokenLimiterConfig :: Count 'PerSecond)
      let microsecondsToWait = ceiling microsecondsToWaitDouble
      -- threadDelay guarantees that _at least_ the given number of microseconds will have passed.
      threadDelay microsecondsToWait
      -- However, it could be MUCH longer than that, so we will recalculate the time instead of
      -- adding that number of microseconds to the old time.
      nowAfterWaiting <- getMonotonicTimeNSec
      -- We do assume here that we will now have enough tokens and do not need to recalculate whether there will be enough.
      -- (We would not know what to do if there weren't, anyway.)
      -- BUT this assumption _should_ hold because _modifyMVar_ guarantees
      -- atomicity if there are no other producers for this MVar, which there
      -- aren't.
      let currentCountAfterWaiting = computeCurrentCount tokenLimiterConfig lastServiced countThen nowAfterWaiting
      pure (lastServicedIsNow nowAfterWaiting, getNewCount currentCountAfterWaiting debit)

-- | Compute the current number of tokens in a bucket purely.
--
-- You should not need this function.
computeCurrentCount :: TokenLimitConfig -> MonotonicTime 'LastServicedTime -> Count 'LastServiced -> MonotonicTime 'Now -> Count 'Current
computeCurrentCount TokenLimitConfig {..} lastServiced countThen now =

  let countToAdd, totalCount :: Word64
      countToAdd = floor $
        fromIntegral (getMonotonicTime now - getMonotonicTime lastServiced :: Word64)
          * fromIntegral (tokenLimitConfigTokensPerSecond :: Count 'PerSecond)
          / 1_000_000_000

      totalCount = getCount countThen + countToAdd

      totalPrecise :: Natural
      totalPrecise = fromIntegral countThen + fromIntegral countToAdd

      willOverflow = totalPrecise > fromIntegral (maxBound :: Word64)

   in Count $ if willOverflow
        then getCount tokenLimitConfigMaxTokens
        else getCount tokenLimitConfigMaxTokens `min` totalCount
