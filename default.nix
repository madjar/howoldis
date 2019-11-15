{ pkgs ? import ./nix { } }:

rec {
  pkgSet = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  };
  haskellNixRoots = pkgs.haskell-nix.haskellNixRoots // {
    ghc-extra-projects = { };
  };
  howoldis = pkgSet.howoldis.components.exes.howoldis;
}
