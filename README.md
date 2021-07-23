# Concurrent Token Bucket-based Rate Limiter

This is an alternative for [`token-limiter`](https://hackage.haskell.org/package/token-limiter) with:

- Thread safety
- No thundering herd problem
- Fairness
- A smaller dependency footprint (only base)
