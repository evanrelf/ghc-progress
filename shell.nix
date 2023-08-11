let
  pkgs = import <nixpkgs> { };

  ghc-progress = pkgs.haskellPackages.callCabal2nix "ghc-progress" ./. { };

in
ghc-progress.env.overrideAttrs (prev: {
  buildInputs = (prev.buildInputs or [ ]) ++ [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
