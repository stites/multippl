{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    naersk.url = "github:nix-community/naersk/master";
    naersk.inputs.nixpkgs.follows = "nixpkgs";

    stenomachines.url = "path:./machines";
    stenomachines.inputs.nixpkgs.follows = "nixpkgs";
    stenomachines.inputs.naersk.follows = "naersk";

    cargo-cabal.url = "github:yvan-sraka/cargo-cabal";
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
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              inherit (inputs.stenomachines.packages.${system}) stenomachines_rs;
            })
          ];
        };
        haskellProjects.default = {
          overrides = final: prev:
            with pkgs.haskell.lib; {
              stenomachines = addPkgconfigDepends (final.callCabal2nix "stenomachines" ./machines {}) (with pkgs; [hidapi systemd.dev]);
              simple-affine-space = prev.simple-affine-space_0_2_1.overrideAttrs (old: {
                doCheck = false;
              });
              dunai = prev.dunai_0_11_0;
              Yampa = prev.Yampa_0_14_2;
            };
          source-overrides = {
            SDL-gfx = "0.6.2.0";
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
          default = pkgs.haskell.lib.addBuildDepends self'.packages.steno (with pkgs; [hidapi systemd.dev]);

          ## Provide a docker image of the binary. Run:
          ##     nix build .#dockerImage
          ##     docker load -i $(nix build .#dockerImage --print-out-paths)
          ## to load this into your docker image registry.
          ## See: https://haskell.flake.page/docker
          #dockerImage = pkgs.dockerTools.buildImage {
          #  name = "steno";
          #  copyToRoot = pkgs.buildEnv {
          #    paths = with pkgs; [
          #      self'.packages.default
          #      # and other conveniences
          #      coreutils
          #      bash
          #      self
          #    ];
          #    name = "steno";
          #    pathsToLink = [ "/bin" ];
          #  };
          #};
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
          nb = {
            description = "run nix build, but also update machines/ (just in case)";
            exec = ''
              nix build --update-input stenomachines
            '';
            category = "Dev Tools";
          };
          refresh-machines = {
            description = "refresh cargo-cabal";
            exec = ''
              cd "$FLAKE_ROOT"/machines
              ${inputs.cargo-cabal.defaultPackage.${system}}/bin/cargo-cabal cabal clean
              ${inputs.cargo-cabal.defaultPackage.${system}}/bin/cargo-cabal cabal init --enable-nix
              ls "$PWD"
              echo "all done!"
            '';
            category = "Dev Tools";
          };
          clang = {
            description = "clang";
            exec = with pkgs; ''
              echo ${clang}/bin/clang -I${hidapi}/include/hidapi -L${hidapi}/lib -L${systemd.dev}/lib -lhidapi-hidraw "$@"
              ${clang}/bin/clang -I${hidapi}/include/hidapi -I${hidapi}/lib -I${systemd.dev}/lib -lhidapi-hidraw "$@"
            '';
            category = "Dev Tools";
          };
          cargo-cabal = {
            description = "cargo-cabal";
            exec = ''
              cd "$FLAKE_ROOT"/machines
              ${inputs.cargo-cabal.defaultPackage.${system}}/bin/cargo-cabal "$@"
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cd "$FLAKE_ROOT"/steno
              cabal v2-repl "$@"
            '';
            category = "Dev Tools";
          };
          dev = {
            description = "Start watchexec";
            exec = ''
              watchexec "cabal v2-run steno -- $*"
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
          languages.rust.enable = true;
          packages = with pkgs; [
            # rust dependencies
            pkg-config
            hidapi
            systemd.dev
          ];
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
