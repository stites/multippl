{
  nixConfig.extra-substituters = "https://stites.cachix.org";
  nixConfig.extra-trusted-public-keys = "stites.cachix.org-1:ZuZInLV0i4TjoZhdh0pr9TFl2OFtHoSnOf4vKqwpthQ=";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";
    crane.url = "github:ipetkov/crane/v0.16.2";
    cachix-push.url = "github:juspay/cachix-push";
    #ppls.url = "path:/home/stites/git/multilang/ppl-benchmarks";
    rsdd.url = "github:stites/rsdd/yodel-additions?dir=nix";
    ppls.url = "git+ssh://git@gitlab.com/stites/ppl-benchmarks";

    # clean up dependencies
    pre-commit.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit.inputs.nixpkgs.follows = "nixpkgs";
    rsdd.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    flake-parts,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit.flakeModule
        inputs.devshell.flakeModule
        inputs.cachix-push.flakeModule
      ];
      flake = {};
      systems = ["x86_64-linux"];
      perSystem = {
        inputs',
        config,
        system,
        pkgs,
        ...
      }: let
        craneLib = (inputs.crane.mkLib pkgs).overrideScope' (final: prev: {
          rsdd = inputs.rsdd.packages.${system}.rsdd;
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
              #tree-sitter
            ]
            ++ lib.optionals pkgs.stdenv.isDarwin [
              pkgs.libiconv
            ];
        };
        # Build *just* the cargo dependencies, so we can reuse
        # all of that work (e.g. via cachix) when running in CI
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        cargoDevArtifacts = craneLib.buildDepsOnly (commonArgs // {CARGO_PROFILE = "dev";});

        # set doCheck = false and use nextest via `nix flake check`
        my-dev-crate = craneLib.buildPackage (commonArgs
          // {
            CARGO_PROFILE = "dev";
            NIX_DEBUG = 0;
            doCheck = false;
          });
        my-crate = craneLib.buildPackage (commonArgs
          // {
            NIX_DEBUG = 0;
            doCheck = false;
          });
      in {
        checks = import ./nix/checks.nix {inherit lib system my-crate craneLib commonArgs cargoArtifacts;};
        packages.default = my-crate;
        packages.dice = inputs'.ppls.packages.dice;
        packages.psi = inputs'.ppls.packages.psi;
        packages.dev = my-dev-crate;
        packages.dev-test = craneLib.cargoNextest (commonArgs
          // {
            cargoArtifacts = cargoDevArtifacts;
            CARGO_PROFILE = "dev";
            cargoNextestExtraArgs = "test_current --nocapture";
            partitions = 1;
            partitionType = "count";
          });

        pre-commit.check.enable = false; # still need to download rsdd from github in offline mode, not sure how to do that right now
        pre-commit.settings.hooks = {
          shellcheck.enable = true;
          clippy.enable = true;
          hunspell.enable = false;
          alejandra.enable = true; # nix formatter
          rustfmt.enable = true;
          typos.enable = true;
        };
        cachix-push.cacheName = "stites";
        devshells.default.commands = [
          {
            name = "repl";
            help = "rust repl with evcxr";
            command = "${pkgs.evcxr}/bin/evcxr";
          }
          {
            name = "ts2emacs";
            help = "compile tree-sitter-yodel.so to ~/.emacs.d/tree-sitter/";
            # https://github.com/tree-sitter/tree-sitter/discussions/1711
            # https://github.com/tree-sitter/tree-sitter/discussions/1711
            command = let
              cmd = dir: ''
                echo "generating yodel.so in user-emacs-directory: ${dir}"
                cd tree-sitter-yodel
                tree-sitter generate -b --libdir ${dir}/tree-sitter/
                echo "renaming yodel.so -> libtree-sitter-yodel.so"
                mv ${dir}/tree-sitter/{,libtree-sitter-}yodel.so
                echo "ls ${dir}/tree-sitter/"
                ls ${dir}/tree-sitter/
              '';
              # in cmd "~/.emacs.d"; # vanilla emacs setup
            in
              cmd "~/.connfig/emacs/.local/cache"; # doom emacs setup
          }
        ];
        devshells.default.devshell.startup.install-pre-commit-hooks.text = config.pre-commit.devShell.shellHook;
        devshells.default.env = [
          # apparently this makes the pre-commit hooks cry :' (
          #{
          #  name = "CARGO_REGISTRIES_CRATES_IO_PROTOCOL";
          #  value = "sparse";
          #}
        ];
        devshells.default.packages = with pkgs;
          [
            (python3.withPackages (p: [p.pyro-ppl p.ipython p.rich] ++ p.pyro-ppl.optional-dependencies.extras))

            config.packages.dice
            config.packages.psi
          ]
          ++ [
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
            valgrind
            cargo-valgrind

            cargo-tarpaulin # code coverage

            # tree-sitter-specific
            tree-sitter

            # polish tools: from https://ectobit.com/blog/speed-up-github-actions-rust-pipelines/
            cargo-deny # lint dependencies
            cargo-outdated # find out outdated dependencies
            cargo-udeps # find out unused dependencies
            cargo-audit # search for security vulnerabilities
            cargo-flamegraph # flamegraph profiling
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
