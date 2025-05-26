{
  description = "token-limiter-concurrent";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    nixpkgs-24_11.url = "github:NixOS/nixpkgs?ref=nixos-24.11";
    nixpkgs-24_05.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    nixpkgs-23_11.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    horizon-advance.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-advance";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-24_11
    , nixpkgs-24_05
    , nixpkgs-23_11
    , horizon-advance
    , pre-commit-hooks
    , fast-myers-diff
    , validity
    , safe-coloured-text
    , autodocodec
    , sydtest
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs { inherit system; config.allowUnfree = true; config.allowBroken = true; };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        (pkgs.callPackage (opt-env-conf + "/nix/overrides.nix") { })
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      horizonPkgs = horizon-advance.legacyPackages.${system}.extend allOverrides;
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.haskellPackages.token-limiter-concurrent;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs: (haskellPackagesFor nixpkgs).autodocodecRelease;
          allNixpkgs = {
            inherit
              nixpkgs-24_11
              nixpkgs-24_05
              nixpkgs-23_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          release = self.packages.${system}.default;
          forwardCompatibility = horizonPkgs.autodocodecRelease;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              cabal-gild.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "token-limiter-concurrent-shell";
        packages = p: [ p.token-limiter-concurrent ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
