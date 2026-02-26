let
  nixpkgs = import ./nix/nixpkgs.nix;
  packages = import ./nix/packages.nix;

  mapAttrs = nixpkgs.lib.mapAttrs;
in
mapAttrs (name: path: builtins.getAttr name nixpkgs.haskellPackages) packages
