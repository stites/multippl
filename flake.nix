{
  nixConfig.extra-substituters = "https://stites.cachix.org";
  nixConfig.extra-trusted-public-keys = "stites.cachix.org-1:ZuZInLV0i4TjoZhdh0pr9TFl2OFtHoSnOf4vKqwpthQ=";

  inputs = {
    devenv.url = "github:cachix/devenv";
    crane.url = "github:ipetkov/crane/v0.14.1";
    rsdd.url = "github:stites/rsdd/yodel-additions?dir=nix";
    # rsdd.url = "path:/home/stites/git/rust/rsdd/nix";
    advisory-db.url = "github:rustsec/advisory-db";
    advisory-db.flake = false;

    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";

    #nixlib.url = "github:stites/nixlib";
    nixlib.url = "path:/home/stites/git/nix/nixlib";

    # clean up dependencies
    nixpkgs.follows = "crane/nixpkgs";
    devenv.inputs.flake-compat.follows = "crane/flake-compat";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    devenv.inputs.pre-commit-hooks.follows = "pre-commit";
    pre-commit.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit.inputs.flake-utils.follows = "crane/flake-utils";
    pre-commit.inputs.flake-compat.follows = "crane/flake-compat";
    pre-commit.inputs.nixpkgs.follows = "nixpkgs";
    rsdd.inputs.devenv.follows = "devenv";
    rsdd.inputs.flake-utils.follows = "crane/flake-utils";
    rsdd.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    flake-parts,
    ...
  } @ inputs: let
    mk-sys-package = import ./nix/mk-sys-package.nix;
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit.flakeModule
        inputs.devshell.flakeModule
      ];
      flake = {};
      systems = ["x86_64-linux"];
      perSystem = {
        config,
        system,
        pkgs,
        ...
      }: let
        craneLib = (inputs.crane.mkLib pkgs).overrideScope' (final: prev: {
          rsdd = inputs.rsdd.packages.${system}.rsdd;
          # for plotters
          expat-sys = mk-sys-package prev "expat-sys" prev.expat;
          freetype-sys = mk-sys-package prev "freetype-sys" prev.freetype;
          fontconfig-sys = mk-sys-package prev "fontconfig-sys" final.fontconfig;
        });
        inherit (pkgs) lib;
        src = builtins.filterSource (path: type:
          (pkgs.lib.any (suffix: pkgs.lib.hasSuffix suffix (baseNameOf "${path}")) [
            # tree sitter files
            ".json"
            ".js"
            ".c"
            ".cc"
            ".gyp"
            ".h"
          ])
          || (craneLib.filterCargoSources path type))
        ./.;

        commonArgs = {
          inherit src;
          buildInputs = with pkgs;
            [
              tree-sitter
              # plotters needs this for the sys packages
              cmake
              pkg-config
              fontconfig
              freetype
              expat
            ]
            ++ lib.optionals pkgs.stdenv.isDarwin [
              pkgs.libiconv
            ];
        };
        # Build *just* the cargo dependencies, so we can reuse
        # all of that work (e.g. via cachix) when running in CI
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        cargoDevArtifacts = craneLib.buildDepsOnly (commonArgs
          // {
            CARGO_PROFILE = "dev";
          });
        my-dev-crate = craneLib.buildPackage (commonArgs
          // {
            CARGO_PROFILE = "dev";
            NIX_DEBUG = 0;
            doCheck = false; # use nextest in `nix flake check` for tests
          });
        my-crate = craneLib.buildPackage (commonArgs
          // {
            NIX_DEBUG = 0;
            doCheck = false; # use nextest in `nix flake check` for tests
          });
      in {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [(import ./nix/fontconfig-overlay.nix)];
        };
        checks = import ./nix/checks.nix {inherit lib system my-crate craneLib commonArgs cargoArtifacts;};
        packages.default = my-crate;
        packages.dev = my-dev-crate;
        packages.dev-test = craneLib.cargoNextest (commonArgs
          // {
            cargoArtifacts = cargoDevArtifacts;
            CARGO_PROFILE = "dev";
            cargoNextestExtraArgs = "test_current --nocapture";
            partitions = 1;
            partitionType = "count";
          });

        apps = let
          cache = "stites";
        in {
          default = inputs.flake-utils.lib.mkApp {
            drv = my-crate;
          };
          cachix-pull = pkgs.callPackage inputs.nixlib.lib.my.apps.cachix-pull {};
          cachix-push = pkgs.callPackage inputs.nixlib.lib.my.apps.cachix-push {
            cache = "stites";
          };
        };
        #devShells.default = import ./nix/shell.nix {inherit inputs pkgs lib;};
        pre-commit.settings.hooks = {
          shellcheck.enable = true;
          clippy.enable = true;
          hunspell.enable = true;
          alejandra.enable = true; # nix formatter
          # statix.enable = true; # lints for nix, but apparently borked
          rustfmt.enable = true;
          typos.enable = true;
        };
        #devshells.default.languages.rust.enableDefaultToolchain = true;
        #devshells.default.languages.rust.packageSet = pkgs.rustPlatform;
        #devshells.default.languages.rust.tools = ["rustc" "cargo" "clippy" "rustfmt"];
        devshells.default.commands = [
          {
            name = "repl";
            help = "rust repl with evcxr";
            command = "${pkgs.evcxr}/bin/evcxr";
          }
        ];
        devshells.default.env = [
          # apparently this makes the pre-commit hooks cry :' (
          #{
          #  name = "CARGO_REGISTRIES_CRATES_IO_PROTOCOL";
          #  value = "sparse";
          #}
        ];
        devshells.default.packages = with pkgs;
          [
            # plotters dependencies
            zlib.dev
            bzip2.dev
            libpng.dev
            brotli.dev
            cmake
            pkg-config
            freetype
            expat
            fontconfig

            hunspellDicts.en_US-large
          ]
          ++ [
            lldb

            cargo
            clippy
            rustc
            rustfmt
            rust-analyzer
            clippy
            cargo-watch
            cargo-nextest
            cargo-expand # expand macros and inspect the output
            cargo-llvm-lines # count number of lines of LLVM IR of a generic function
            cargo-inspect
            cargo-criterion
            evcxr # make sure repl is in a gc-root
            cargo-play # quickly run a rust file that has a main function

            cargo-tarpaulin # code coverage

            # tree-sitter-specific
            tree-sitter

            # polish tools: from https://ectobit.com/blog/speed-up-github-actions-rust-pipelines/
            cargo-deny # lint dependencies
            cargo-outdated # find out outdated dependencies
            cargo-udeps # find out unused dependencies
            cargo-audit # search for security vulnerabilities
            bacon
            # cargo-pants # search for security vulnerabilities (by sonatype )
          ]
          ++ lib.optionals stdenv.isDarwin []
          ++ lib.optionals stdenv.isLinux [
            cargo-rr
            rr-unstable
          ];
      };
    };
}
