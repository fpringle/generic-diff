let
  sources = import ./sources.nix;
  nixpkgs = import sources.nixpkgs { };
  nix-pre-commit-hooks = import sources.pre-commit-hooks;

  haskell-file-pattern = "\\.l?hs(-boot)?$";
  nix-file-pattern = "\\.nix$";

  hlint = "${nixpkgs.hlint}/bin/hlint";
  fourmolu = "${nixpkgs.haskellPackages.fourmolu}/bin/fourmolu";
  nixpkgs-fmt = "${nixpkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";

  # runAllFiles "foo" will output a hook entry that loops through
  # all the files supplied by pre-commit and call "foo" on all of them.
  # i.e. `runAllFiles` is basically like haskell's `traverse`.
  #
  # The gross bash stuff is splitting the input list of files (which is supplied
  # as a list of arguments to the script) and calling the command on each of them
  # via array indexing (n is the index)
  # 
  # If the hook fails, pre-commit will print everything that the hook printed to
  # stdout. So the last printed file is the one that threw an error.
  forAllFiles = executable:
    ''
      bash -c 'for n in $(seq 0 "$#"); do
        echo "''${!n}"
        ${executable} "''${!n}"
      done'
    '';
in
nix-pre-commit-hooks.run {
  src = ./.;

  hooks = {
    "0-hlint-pinned" = {
      name = "hlint";
      enable = true;
      description =
        "HLint gives suggestions on how to improve your source code.";
      entry = forAllFiles "${hlint} --refactor --refactor-options=\"-i\"";
      files = haskell-file-pattern;
    };

    "1-fourmolu-pinned" = {
      name = "fourmolu";
      enable = true;
      description = "Haskell code prettifier.";
      entry = "${fourmolu} --mode inplace";
      files = haskell-file-pattern;
      excludes = [
        "effectful-postgresql/src/Effectful/PostgreSQL.hs"
      ];
    };

    "2-nixpkgs-format" = {
      name = "nixpkgs-fmt";
      enable = true;
      description = "Nix code formatter for nixpkgs.";
      entry = "${nixpkgs-fmt}";
      files = nix-file-pattern;
    };

    # by default, pre-commit fails if a hook modifies files, but doesn't
    # tell us which files have been modified. Smart, right?
    # this workaround runs a `git diff` to print any files that have
    # been modified by previous hooks.
    # NOTE: this should always be the last hook run, so when adding hooks
    # make sure to add them above this one.
    "3-git-diff" = {
      name = "git diff";
      enable = true;
      entry = "git diff --name-only --exit-code";
      language = "system";
      pass_filenames = true;
    };
  };
}
