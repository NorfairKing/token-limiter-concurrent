{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Concurrent.TokenLimiter.Concurrent
  ( Count,
    TokenLimitConfig (..),
    MonotonicTime,
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
import GHC.Clock
import GHC.Generics (Generic)
import Numeric.Natural

-- | An amount of tokens
type Count = Word64

-- | A configuration for 'TokenLimiter'
data TokenLimitConfig = TokenLimitConfig
  { -- | How many tokens should be in the bucket when it's created
    tokenLimitConfigInitialTokens :: !Count,
    -- | Maximum number of tokens the bucket can hold at any one time
    tokenLimitConfigMaxTokens :: !Count,
    -- | How many tokens are added to the bucket per second
    tokenLimitConfigTokensPerSecond :: !Count
  }
  deriving (Show, Eq, Generic)

-- | A type synonym for a number of "monotonic time" nanoseconds.
--
-- This only exists because it is also a 'Word64' and would be too easy to confuse with a 'Count'.
type MonotonicTime = Word64

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
    tokenLimiterLastServiced :: !(MVar (MonotonicTime, Count))
  }
  deriving (Eq, Generic)

-- | Make a token limiter
--
-- The initial number of tokens will be the minimum of the 'tokenLimitConfigInitialTokens' and the 'tokenLimitConfigMaxTokens',
makeTokenLimiter :: TokenLimitConfig -> IO TokenLimiter
makeTokenLimiter tokenLimiterConfig = do
  now <- getMonotonicTimeNSec
  tokenLimiterLastServiced <- newMVar (now, min (tokenLimitConfigInitialTokens tokenLimiterConfig) (tokenLimitConfigMaxTokens tokenLimiterConfig))
  pure TokenLimiter {..}

-- | Ask if we could debit a number of tokens, without actually doing it.
--
-- Note that this information can become stale _very_ quickly.
-- If you want to also actually debit a number of tokens, use 'tryDebit' instead.
--
-- Warning: this function can block for long if there are other threads waiting to debit tokens.
-- Use in conjunction with an Async.race timeout or similar if you want to avoid lengthy blocking.
canDebit :: TokenLimiter -> Word64 -> IO Bool
canDebit TokenLimiter {..} debit = withMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  let enoughAvailable = currentCount >= debit
  pure enoughAvailable

-- | Check if we can debit a number of tokens, and do it if possible.
--
-- The returned boolean represents whether the tokens were debited.
--
-- Warning: blocking caveat described in 'canDebit' applies here too.
tryDebit :: TokenLimiter -> Word64 -> IO Bool
tryDebit TokenLimiter {..} debit = modifyMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  let enoughAvailable = currentCount >= debit
  if enoughAvailable
    then do
      let newCount = currentCount - debit
      pure ((now, newCount), True)
    else pure ((lastServiced, countThen), False)

-- | Wait until the given number of tokens can be debited.
--
-- Note: debitor threads are serviced in FIFO order, so a request for a small
-- (and currently satisfiable) number of tokens can still be delayed by a debit
-- request for a larger amount of tokens.
waitDebit :: TokenLimiter -> Word64 -> IO ()
waitDebit TokenLimiter {..} debit = modifyMVar_ tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  let enoughAvailable = currentCount >= debit
  if enoughAvailable
    then do
      let newCount = currentCount - debit
      pure (now, newCount)
    else do
      let extraTokensNeeded = debit - currentCount
      let microsecondsToWaitDouble :: Double
          microsecondsToWaitDouble =
            1_000_000
              -- fromIntegral :: Word64 -> Double
              * fromIntegral extraTokensNeeded
              -- fromIntegral :: Word64 -> Double
              / fromIntegral (tokenLimitConfigTokensPerSecond tokenLimiterConfig)
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
      let newCount = currentCountAfterWaiting - debit
      pure (nowAfterWaiting, newCount)

-- | Compute the current number of tokens in a bucket purely.
--
-- You should not need this function.
computeCurrentCount :: TokenLimitConfig -> MonotonicTime -> Count -> MonotonicTime -> Count
computeCurrentCount TokenLimitConfig {..} lastServiced countThen now =
  let nanoDiff :: Word64
      nanoDiff = now - lastServiced
      countToAddDouble :: Double
      countToAddDouble =
        -- fromIntegral :: Word64 -> Double
        fromIntegral nanoDiff
          -- fromIntegral :: Word64 -> Double
          * fromIntegral tokenLimitConfigTokensPerSecond
          / 1_000_000_000
      countToAdd :: Word64
      countToAdd = floor countToAddDouble
      totalPrecise :: Natural
      totalPrecise = fromIntegral countThen + fromIntegral countToAdd
      willOverflow = totalPrecise > fromIntegral (maxBound :: Word64)
      totalCount = countThen + countToAdd
   in if willOverflow
        then tokenLimitConfigMaxTokens
        else min tokenLimitConfigMaxTokens totalCount
