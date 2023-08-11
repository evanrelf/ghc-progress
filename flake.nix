{
  description = "ghc-progress";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (final: prev: {
            haskellPackages =
              prev.haskellPackages.override {
                overrides = hfinal: hprev: {
                  flatparse = pkgs.haskellPackages.flatparse_0_4_1_0;
                };
              };
          })
        ];

        pkgs = import nixpkgs { inherit overlays system; };
      in
      rec {
        packages = rec {
          default = ghc-progress;
          ghc-progress =
            let
              drv = pkgs.haskellPackages.callCabal2nix "ghc-progress" ./. { };
            in
            pkgs.lib.pipe drv [
              (pkgs.haskell.lib.compose.addBuildDepend pkgs.llvm)
              (pkgs.haskell.lib.compose.appendConfigureFlag "-ffast")
            ];
        };

        devShells = {
          default =
            packages.ghc-progress.env.overrideAttrs (prev: {
              buildInputs = [
                pkgs.cabal-install
                pkgs.ghcid
              ];
            });
        };
      }
    );
}
