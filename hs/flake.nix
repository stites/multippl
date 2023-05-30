{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    check-flake.url = "github:srid/check-flake";
    mission-control.url = "github:Platonic-Systems/mission-control";
    devenv.url = "github:cachix/devenv";
    #machines.url = ./machines;
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.check-flake.flakeModule
        inputs.devenv.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = {
        self',
        config,
        pkgs,
        system,
        ...
      }: {
        _module.args.pkgs = inputs.nixpkgs.legacyPackages.${system};
        haskellProjects.default = {
          packages = {
            yodel.root = ./.;
          };
          devShell = {
            tools = hp:
              {
                treefmt = config.treefmt.build.wrapper;
              }
              // config.treefmt.build.programs;
            hlsCheck.enable = true;
          };
          autoWire = ["packages" "apps" "checks"]; # Wire all but the devShell
        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };

        packages = {
          default = self'.packages.yodel;
        };
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };

          repl = {
            description = "Start the cabal repl";
            exec = ''
              cd "$FLAKE_ROOT"/hs
              cabal v2-repl "$@"
            '';
            category = "Dev Tools";
          };
          reload = {
            description = "run direnv reload";
            exec = ''
              direnv reload
            '';
            category = "Dev Tools";
          };
          dev = {
            description = "Start watchexec";
            exec = ''
              watchexec -w src/ -w yodel.cabal "cabal v2-build yodel -- $*"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
        };
        devenv.shells.default = {
          #languages.haskell.enable = true;
        };
        devShells.default = with pkgs;
          lib.mkForce (mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.flake-root.devShell
              config.mission-control.devShell
              config.devenv.shells.default
            ];
            # devenv puts packages here
            buildInputs = config.devenv.shells.default.packages;
          });
      };
    };
}
