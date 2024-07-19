{ lib
, haskell
, symlinkJoin
, ...
}:
with lib;
with haskell.lib;
self: super: {
  token-limiter-concurrent = buildStrictly (self.callPackage ../token-limiter-concurrent { });
}
