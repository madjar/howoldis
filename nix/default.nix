{ sources ? import ./sources.nix }:
let
  overlay = _: pkgs: {
    haskellnix = import sources."haskell.nix" { inherit pkgs; };
    inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
  };
in import sources.nixpkgs { overlays = [ overlay ]; }
