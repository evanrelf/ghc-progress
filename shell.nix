let
  pkgs = import <nixpkgs> { };

  ghc-progress = pkgs.haskellPackages.callCabal2nix "ghc-progress" ./. {
    flatparse = pkgs.haskellPackages.flatparse_0_4_1_0;
  };

in
ghc-progress.env.overrideAttrs (prev: {
  buildInputs = (prev.buildInputs or [ ]) ++ [
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.llvm
  ];
})
