{ pkgs ? import ./nixpkgs.nix }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  haskellnix = import (builtins.fetchTarball
    "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {
      inherit pkgs;
    };
  pkgSet = haskellnix.mkStackPkgSet {
    stack-pkgs = (haskellnix.importAndFilterProject
      (haskellnix.callStackToNix { inherit src; })).pkgs;
  };
  packages = pkgSet.config.hsPkgs;
in { howoldis = packages.howoldis.components.exes.howoldis; }
