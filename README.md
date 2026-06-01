# Concurrent Token Bucket-based Rate Limiter

[![NixCI](https://staging.nix-ci.com/badge/gh:NorfairKing:token-limiter-concurrent)](https://staging.nix-ci.com/gh:NorfairKing:token-limiter-concurrent)

This is an alternative for [`token-limiter`](https://hackage.haskell.org/package/token-limiter) with:

- Thread safety
- No thundering herd problem
- Fairness
- A smaller dependency footprint (only base)
