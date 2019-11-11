let
  pkgs = import <nixpkgs> {
  };
  hspkgs = pkgs.haskell.packages.ghc843;
in
{
  vtte = hspkgs.callPackage ./derivation.nix {};
}
