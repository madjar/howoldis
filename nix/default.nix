{ sources ? import ./sources.nix }:
import sources.nixpkgs (import sources."haskell.nix")
