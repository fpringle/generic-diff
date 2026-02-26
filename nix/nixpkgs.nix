let
  # set the GHC version for the entire project.
  ghc-version = "ghc984";

  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { overlays = [ overlay ]; };
  packages = import ./packages.nix;

  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ../.gitignore ];

  overlay = final: prev:
    let
      inherit (prev.haskell.lib)
        doJailbreak
        doCheck
        ;

      haskell-overrides = hfinal: hprev:
        let
          # When we pin specific versions of Haskell packages, they'll go here using callCabal2Nix.
          packageOverrides = {
            # postgresql-operation-counting = hfinal.callCabal2nix "postgresql-operation-counting" sources.postgresql-operation-counting { };
            # postgresql-operation-counting = hfinal.callCabal2nix "postgresql-operation-counting" "${sources.postgresql-operation-counting}" { };
            postgresql-operation-counting = import "${sources.postgresql-operation-counting}";
          };

          makePackage = name: path:
            let pkg = hfinal.callCabal2nix name (gitignore path) { };
            in doCheck pkg;
        in
        builtins.mapAttrs makePackage packages // packageOverrides;
    in
    {
      # This will become the main package set for the project. In a `nix-shell`,
      # this is what we'll have access to.
      haskellPackages = prev.haskell.packages.${ghc-version}.override {
        overrides = haskell-overrides;
      };
    };
in
nixpkgs
