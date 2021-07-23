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

type Count = Word64

data TokenLimitConfig = TokenLimitConfig
  { -- | how many tokens should be in the bucket when it's created.
    tokenLimitConfigInitialTokens :: !Count,
    -- | maximum number of tokens the bucket can hold at any one time.
    tokenLimitConfigMaxTokens :: !Count,
    -- | How many tokens are added to the bucket per second
    tokenLimitConfigTokensPerSecond :: !Count
  }
  deriving (Show, Eq, Generic)

type MonotonicTime = Word64

data TokenLimiter = TokenLimiter
  { tokenLimiterConfig :: !TokenLimitConfig,
    tokenLimiterLastServiced :: !(MVar (MonotonicTime, Count)) -- Time and count at that time
  }
  deriving (Eq, Generic)

-- | Make a token limiter
--
-- This token limiter is thread-safe and guarantees that there will be no thundering herd problem.
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
canDebit :: TokenLimiter -> Word64 -> IO Bool
canDebit TokenLimiter {..} debit = withMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  let enoughAvailable = currentCount >= debit
  pure enoughAvailable

-- | Check if we can debit a number of tokens, and do it if possible.
--
-- The returned boolean represents whether the tokens were debited.
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

-- | Wait until the given number of tokens can be debited
waitDebit :: TokenLimiter -> Word64 -> IO ()
waitDebit TokenLimiter {..} debit = modifyMVar tokenLimiterLastServiced $ \(lastServiced, countThen) -> do
  now <- getMonotonicTimeNSec
  let currentCount = computeCurrentCount tokenLimiterConfig lastServiced countThen now
  let enoughAvailable = currentCount >= debit
  if enoughAvailable
    then do
      let newCount = currentCount - debit
      pure ((now, newCount), ())
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
      threadDelay microsecondsToWait
      nowAfterWaiting <- getMonotonicTimeNSec
      let currentCountAfterWaiting = computeCurrentCount tokenLimiterConfig lastServiced countThen nowAfterWaiting
      let newCount = currentCountAfterWaiting - debit
      pure ((nowAfterWaiting, newCount), ())

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
