let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  inherit (import ./utils.nix) findHaskellPackages;
in
findHaskellPackages ../.
