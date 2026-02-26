{ packages ? ""
}:
let
  nixpkgs = import ./nix/nixpkgs.nix;
  pre-commit-check = import ./nix/pre-commit.nix;
  monorepo = import ./generic-diff.nix;
  allPackages = builtins.attrNames monorepo;
  shell-packages =
    if packages == ""
    then allPackages
    else nixpkgs.lib.strings.splitString "," packages;
in
with nixpkgs;
with nixpkgs.haskellPackages;
shellFor {
  packages = p: nixpkgs.lib.attrVals shell-packages p;
  buildInputs = [
    cabal-install
    haskell-language-server
    hlint
    fourmolu
    ghcid
    niv
    nixpkgs-fmt
    pre-commit
  ];
  shellHook = ''
    ${pre-commit-check.shellHook}
  '';
}
