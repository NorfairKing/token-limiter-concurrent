{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Concurrent.Tokenlimiter.ConcurrentSpec (spec) where

import Control.Concurrent.Async
import Control.Concurrent.Tokenlimiter.Concurrent
import Control.Monad
import Data.Word
import GHC.Clock
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance Validity TokenLimitConfig

instance GenValid TokenLimitConfig where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
  describe "computeCurrentCount" $ do
    it "works in this super simple case" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 0,
                tokenLimitConfigMaxTokens = 1,
                tokenLimitConfigTokensPerSecond = 1
              }
      -- One nanosecond later we shouldn't have any more tokens
      computeCurrentCount config 0 0 1 `shouldBe` 0

    it "Does not need to wait whole seconds to work" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 0,
                tokenLimitConfigMaxTokens = 1000,
                tokenLimitConfigTokensPerSecond = 1000
              }
      -- After half a second we should have 500 tokens
      computeCurrentCount config 0 0 500_000_000 `shouldBe` 500

    it "Does not go over the maximum" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 0,
                tokenLimitConfigMaxTokens = 10,
                tokenLimitConfigTokensPerSecond = 10
              }
      -- One nanosecond later we shouldn't have any more tokens
      computeCurrentCount config 0 0 2_000_000_000 `shouldBe` 10

    it "Can deal with overflow" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = maxBound -1,
                tokenLimitConfigMaxTokens = maxBound,
                tokenLimitConfigTokensPerSecond = 1
              }
      -- 10 seconds later we should have no more than maxBound, eventhough the computed count would be maxBound + 9
      computeCurrentCount config 0 maxBound 10_000_000_000 `shouldBe` maxBound

  describe "makeTokenLimiter" $ do
    it "always succeeds" $
      forAllValid $ \config -> do
        tokenLimiter <- makeTokenLimiter config
        tokenLimiterConfig tokenLimiter `shouldBe` config

  describe "canDebit" $ do
    it "is correct for this simple example" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 10,
                tokenLimitConfigMaxTokens = 20,
                tokenLimitConfigTokensPerSecond = 1
              }
      limiter <- makeTokenLimiter config
      canDebit limiter 10 `shouldReturn` True

    it "says true when tokens are available from the start and false otherwise" $
      forAllValid $ \config ->
        forAllValid $ \needed -> do
          -- We set the tokens per second to 1 because otherwise the generated
          -- value can be large enough for the time difference between making
          -- the token limiter and running 'canDebit' to matter.
          limiter <- makeTokenLimiter config {tokenLimitConfigTokensPerSecond = 1}
          couldDebit <- canDebit limiter needed
          couldDebit `shouldBe` needed <= min (tokenLimitConfigInitialTokens config) (tokenLimitConfigMaxTokens config)

  describe "tryDebit" $ do
    it "is correct for this simple example" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 10,
                tokenLimitConfigMaxTokens = 20,
                tokenLimitConfigTokensPerSecond = 1
              }
      limiter <- makeTokenLimiter config
      tryDebit limiter 10 `shouldReturn` True

    it "says true when tokens are available from the start and false otherwise" $
      forAllValid $ \config ->
        forAllValid $ \needed -> do
          -- We set the tokens per second to 1 because otherwise the generated
          -- value can be large enough for the time difference between making
          -- the token limiter and running 'tryDebit' to matter.
          limiter <- makeTokenLimiter config {tokenLimitConfigTokensPerSecond = 1}
          didDebit <- tryDebit limiter needed
          didDebit `shouldBe` needed <= min (tokenLimitConfigInitialTokens config) (tokenLimitConfigMaxTokens config)

    it "always says true if canDebit said true first and there are no other threads" $
      forAllValid $ \config ->
        forAllValid $ \needed -> do
          limiter <- makeTokenLimiter config
          couldDebit <- canDebit limiter needed
          when couldDebit $ tryDebit limiter needed `shouldReturn` True

  describe "waitDebit" $ do
    it "does not need to wait for this simple example" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 10,
                tokenLimitConfigMaxTokens = 20,
                tokenLimitConfigTokensPerSecond = 1
              }
      limiter <- makeTokenLimiter config
      nanos <- time_ $ waitDebit limiter 10
      nanos `shouldSatisfy` (< 1_000_000_000)

    it "Does not need to wait if tokens are available" $
      forAllValid $ \initial ->
        forAllValid $ \maxTokens -> do
          let config =
                TokenLimitConfig
                  { tokenLimitConfigInitialTokens = initial,
                    tokenLimitConfigMaxTokens = maxTokens,
                    tokenLimitConfigTokensPerSecond = 1
                  }
          limiter <- makeTokenLimiter config
          let needed = min initial maxTokens
          nanos <- time_ $ waitDebit limiter needed
          nanos `shouldSatisfy` (< 1_000_000_000)

    it "Waits appropriately when there is one threads that want tokens in this example." $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 0,
                tokenLimitConfigMaxTokens = 1,
                tokenLimitConfigTokensPerSecond = 1
              }
      limiter <- makeTokenLimiter config
      nanos <- time_ $ waitDebit limiter 1
      nanos `shouldSatisfy` (>= 1_000_000_000)

    it "does not need to wait a whole number of seconds" $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 0,
                tokenLimitConfigMaxTokens = 10,
                tokenLimitConfigTokensPerSecond = 10
              }
      limiter <- makeTokenLimiter config
      nanos <- time_ $ waitDebit limiter 1
      nanos `shouldSatisfy` (<= 500_000_000)

    it "Waits appropriately when there are multiple threads that want tokens at the same time in this example." $ do
      let config =
            TokenLimitConfig
              { tokenLimitConfigInitialTokens = 0,
                tokenLimitConfigMaxTokens = 1,
                tokenLimitConfigTokensPerSecond = 1
              }
      limiter <- makeTokenLimiter config
      nanos <- time_ $ concurrently_ (waitDebit limiter 1) (waitDebit limiter 1)
      nanos `shouldSatisfy` (>= 2_000_000_000)

    modifyMaxSuccess (`div` 50) $
      it "Waits appropriately when there are many threads that want tokens at the same time in this example." $ do
        forAll (sized pure) $ \numberOfThreads -> do
          print numberOfThreads
          let config =
                TokenLimitConfig
                  { tokenLimitConfigInitialTokens = 0,
                    tokenLimitConfigMaxTokens = 1,
                    tokenLimitConfigTokensPerSecond = 100
                  }
          limiter <- makeTokenLimiter config
          nanos <- time_ $ replicateConcurrently_ numberOfThreads (waitDebit limiter 1)
          nanos `shouldSatisfy` (>= fromIntegral numberOfThreads * 10_000_000)

time_ :: IO a -> IO Word64
time_ func = snd <$> time func

time :: IO a -> IO (a, Word64)
time func = do
  begin <- getMonotonicTimeNSec
  result <- func
  end <- getMonotonicTimeNSec
  pure (result, end - begin)
