{-# LANGUAGE NumericUnderscores #-}

module Control.Concurrent.Tokenlimiter.ConcurrentSpec (spec) where

import Control.Concurrent.Tokenlimiter.Concurrent
import Control.Monad
import Data.Word
import GHC.Clock
import Test.Syd
import Test.Syd.Validity

instance Validity TokenLimitConfig

instance GenValid TokenLimitConfig where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
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
          limiter <- makeTokenLimiter config
          couldDebit <- canDebit limiter needed
          couldDebit `shouldBe` needed <= max (tokenLimitConfigInitialTokens config) (tokenLimitConfigMaxTokens config)

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
          limiter <- makeTokenLimiter config
          didDebit <- tryDebit limiter needed
          didDebit `shouldBe` needed <= max (tokenLimitConfigInitialTokens config) (tokenLimitConfigMaxTokens config)

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
          let needed = initial
          nanos <- time_ $ waitDebit limiter needed
          nanos `shouldSatisfy` (< 1_000_000_000)

time_ :: IO a -> IO Word64
time_ func = snd <$> time func

time :: IO a -> IO (a, Word64)
time func = do
  begin <- getMonotonicTimeNSec
  result <- func
  end <- getMonotonicTimeNSec
  pure (result, end - begin)
