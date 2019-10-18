{ pkgs ? import ./nix { } }:
let
  pkgSet = with pkgs.haskellnix;
    mkStackPkgSet {
      stack-pkgs = (importAndFilterProject
        (callStackToNix { src = pkgs.gitignoreSource ./.; })).pkgs;
    };
  packages = pkgSet.config.hsPkgs;
in { howoldis = packages.howoldis.components.exes.howoldis; }
