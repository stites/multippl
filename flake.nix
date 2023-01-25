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
        } ];
      };
    });
}
