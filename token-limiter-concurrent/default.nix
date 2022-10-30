{ mkDerivation, async, base, genvalidity, genvalidity-sydtest, lib
, QuickCheck, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "token-limiter-concurrent";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    async base genvalidity genvalidity-sydtest QuickCheck sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/token-limiter-concurrent#readme";
  description = "A thread-safe concurrent token-bucket rate limiter that guarantees fairness";
  license = lib.licenses.mit;
}
