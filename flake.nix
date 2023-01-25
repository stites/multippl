{
  nixConfig.trusted-substituters = "https://lean4.cachix.org/";
  nixConfig.trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= lean4.cachix.org-1:mawtxSxcaiWE24xCXXgh3qnvlTkyU7evRRnGeAhD4Wk=";
  nixConfig.max-jobs = "auto";  # Allow building multiple derivations in parallel
  nixConfig.keep-outputs = true;  # Do not garbage-collect build time-only dependencies (e.g. clang)

  description = "Advent of code";

  inputs = {
    lean.url = "github:leanprover/lean4";
    nixpkgs.follows = "lean/nixpkgs";
    flake-utils.follows = "lean/flake-utils";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs@{ self, lean, ... }: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # lean infrastructure:
      # =========================================
      leanPkgs = lean.packages.${system};
      pkgs = leanPkgs.pkgs;
      myLeanPkg = leanPkgs.buildLeanPackage {
        name = "AdventOfCode";
        src = ./.;
      };
    in {
      packages = myLeanPkg // {
        inherit (leanPkgs) lean;
        default = myLeanPkg.modRoot;
      };

      devShells.default = inputs.devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [ {
          # see https://devenv.sh/reference/options/
          # languages.nix.enable = true;
          # difftastic.enable = true; # https://devenv.sh/integrations/difftastic/
          # packages = with pkgs; [ leanPkgs.lean ];


          ## https://devenv.sh/basics/
          #env.GREET = "devenv";
          #
          ## https://devenv.sh/scripts/
          #scripts.hello.exec = "echo hello from $GREET";
          #enterShell = ''
          #  hello
          #'';
          ## https://devenv.sh/pre-commit-hooks/
          #pre-commit.hooks.shellcheck.enable = true;

          ## # taken from devshell.toml
          ## devshell.packages = ["nixpkgs-fmt", "vscode-dev"]
          ## 
          ## # general tools
          ## [[commands]]
          ## package = "devshell.cli"
          ## help = "Per project developer environments"
          ## 
          ## [[commands]]
          ## package = "lldb"
          ## 
          ## [[commands]]
          ## package = "rr-unstable"
          ## 
          ## # lean dependencies
          ## [[commands]]
          ## package = "lean-bin-dev"
          ## help = "A functional programming language that makes it easy to write correct and maintainable code"
          ## category = "lean"
          ## 
          ## [[commands]]
          ## package = "lake-dev"
          ## category = "lean"
          ## 
          ## [[commands]]
          ## package = "emacs-dev"
          ## help = "open a pinned version of Emacs with lean4-mode fully set up"
          ## category = "lean"
          ## 
          ## [[commands]]
          ## command = "code"
          ## name = "vscode-dev"
          ## help = "open a pinned version of VSCode with a lean4 set up"
          ## category = "lean"
          ## 
          ## # taken from Mathlib, but not applicable in advent of code
          ## # [[commands]]
          ## # name = "mk-adventofcode"
          ## # command = """
          ## # cd $(git rev-parse --show-toplevel)
          ## # find . -name '*.lean' -not -name 'AdventOfCode.lean' | env LC_ALL=C sort | cut -d '/' -f 2- | sed 's/\\.lean//;s,/,.,g;s/^/import /' > AdventOfCode.lean
          ## # """
          ## # help = "Add all new *.lean files to AdventOfCode.lean"
          ## # category = "lean"

        } ];
      };
    });
}
