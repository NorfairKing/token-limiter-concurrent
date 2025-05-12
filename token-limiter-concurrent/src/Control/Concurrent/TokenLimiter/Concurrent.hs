{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Concurrent.TokenLimiter.Concurrent
  ( -- * Create
    Count,
    TokenLimitConfig (..),
    MonotonicTime,
    TokenLimiter (..),
    makeTokenLimiter,

    -- * Use
    tryDebit,
    waitDebit,
    MonotonicDiffNanos (..),

    -- * Helper functions
    computeMicrosecondsToWait,
    computeCurrentCount,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe (fromMaybe)
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

newtype MonotonicDiffNanos = MonotonicDiffNanos {unMonotonicDiffNanos :: Word64}
  deriving (Show, Eq, Ord, Generic)

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

-- | Check if we can debit a number of tokens, and do it if possible.
--
-- The returned boolean represents whether the tokens were debited.
--
-- Note that there is a small race-condition in which `tryDebit` sometimes
-- returns `False` eventhough it could (maybe) have debited because another
-- thread was currently `waitDebit`-ing without actually waiting (because it
-- didn't need to wait).
tryDebit :: TokenLimiter -> Word64 -> IO Bool
tryDebit TokenLimiter {..} debit =
  fmap (fromMaybe False) $
    tryModifyMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
      now <- getMonotonicTimeNSec
      let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
      let enoughAvailable = currentCount >= debit
      pure $
        if enoughAvailable
          then
            let newCount = currentCount - debit
             in ((now, newCount), True)
          else ((lastServiced, countThen), False)

-- | Wait until the given number of tokens can be debited.
--
-- Returns the time waited, for stats recording purposes.
--
-- Note: only reports the time waited due to rate-limiting this specific action,
-- not the wall-clock time waited (that might include waiting for previous
-- actions limited by this rate limiter to finish).
--
-- Note: debitor threads are serviced in FIFO order, so a request for a small
-- (and currently satisfiable) number of tokens can still be delayed by a debit
-- request for a larger amount of tokens.
--
-- Note: the wait time reported can be inflated due to scheduling inaccuracy.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/16601.
waitDebit :: TokenLimiter -> Word64 -> IO (Maybe MonotonicDiffNanos)
waitDebit TokenLimiter {..} debit = modifyMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  let enoughAvailable = currentCount >= debit
  if enoughAvailable
    then do
      let newCount = currentCount - debit
      pure ((now, newCount), Nothing)
    else do
      let extraTokensNeeded = debit - currentCount
      case computeMicrosecondsToWait (tokenLimitConfigTokensPerSecond tokenLimiterConfig) extraTokensNeeded of
        Nothing -> do
          -- We can't wait for this long, so we just wait forever.
          -- This is a bit sad, but it is the best we can do.
          -- We could also throw an exception, but that would be a bit rude.
          -- So we just wait forever.
          pure ((lastServiced, countThen), Nothing)
        Just microsecondsToWait -> do
          -- We can wait for this long, so we will.
          --
          -- threadDelay guarantees that _at least_ the given number of microseconds will have passed.
          threadDelay microsecondsToWait
          -- However, it could be MUCH longer than that, so we will recalculate the time instead of
          -- adding that number of microseconds to the old time.
          nowAfterWaiting <- getMonotonicTimeNSec
          let delta = MonotonicDiffNanos (nowAfterWaiting - now)
          -- We do assume here that we will now have enough tokens and do not need to recalculate whether there will be enough.
          -- (We would not know what to do if there weren't, anyway.)
          -- BUT this assumption _should_ hold because _modifyMVar_ guarantees
          -- atomicity if there are no other producers for this MVar, which there
          -- aren't.
          let currentCountAfterWaiting = computeCurrentCount tokenLimiterConfig lastServiced countThen nowAfterWaiting
          let newCount = currentCountAfterWaiting - debit
          pure ((nowAfterWaiting, newCount), Just delta)

computeMicrosecondsToWait :: Count -> Word64 -> Maybe Int
computeMicrosecondsToWait tokensPerSecond extraTokensNeeded = do
  let microsecondsToWaitDouble :: Double
      microsecondsToWaitDouble =
        1_000_000
          * (fromIntegral :: Word64 -> Double) extraTokensNeeded
          / (fromIntegral :: Word64 -> Double) tokensPerSecond

  let maxBoundDouble :: Double
      maxBoundDouble = fromIntegral (maxBound :: Int)

  guard $ microsecondsToWaitDouble < maxBoundDouble

  pure $ ceiling microsecondsToWaitDouble

waitForever :: IO void
waitForever = forever $ threadDelay 1_000_000

-- | Compute the current number of tokens in a bucket purely.
--
-- You should not need this function.
computeCurrentCount :: TokenLimitConfig -> MonotonicTime -> Count -> MonotonicTime -> Count
computeCurrentCount TokenLimitConfig {..} lastServiced countThen now =
  let nanoDiff :: Word64
      nanoDiff = now - lastServiced
      countToAddDouble :: Double
      countToAddDouble =
        (fromIntegral :: Word64 -> Double) nanoDiff
          * (fromIntegral :: Word64 -> Double) tokenLimitConfigTokensPerSecond
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

tryModifyMVar :: MVar a -> (a -> IO (a, b)) -> IO (Maybe b)
tryModifyMVar m io =
  mask $ \restore -> do
    mA <- tryTakeMVar m
    forM mA $ \a -> do
      (a', b) <-
        restore (io a)
          `onException` putMVar m a
      putMVar m a'
      pure b
