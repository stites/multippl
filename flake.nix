{
  nixConfig.extra-substituters = "https://stites.cachix.org";
  nixConfig.extra-trusted-public-keys = "stites.cachix.org-1:JN1rOOglf6VA+2aFsZnpkGUFfutdBIP1LbANgiJ940s=";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
    crane.url = "github:ipetkov/crane/v0.11.3";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    dice.url = "github:stites/dice.nix";
    rsdd.url = "github:stites/rsdd/yodel-additions?dir=nix";
    # rsdd.url = "path:/home/stites/git/rust/rsdd/nix";
    advisory-db.url = "github:rustsec/advisory-db";
    advisory-db.flake = false;

    # clean up dependencies
    flake-utils.follows = "crane/flake-utils";
    devenv.inputs.flake-compat.follows = "crane/flake-compat";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    devenv.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.flake-utils.follows = "crane/flake-utils";
    pre-commit-hooks.inputs.flake-compat.follows = "crane/flake-compat";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    rsdd.inputs.devenv.follows = "devenv";
    rsdd.inputs.flake-utils.follows = "crane/flake-utils";
    rsdd.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    ...
  } @ inputs: let
    flklib = inputs.flake-utils.lib;
    mk-sys-package = prev: name: _pkg:
      prev.rustBuilder.rustLib.makeOverride {
        inherit name;
        overrideAttrs = drv: {
          propagatedBuildInputs =
            drv.propagatedBuildInputs
            ++ (with prev; [
              cmake
              pkg-config
              _pkg
            ]);
          propagatedNativeBuildInputs =
            (
              if builtins.hasAttr drv "propagatedNativeBuildInputs"
              then drv.propagatedNativeBuildInputs
              else []
            )
            ++ (with prev; [
              cmake
              pkg-config
              _pkg
            ]);
        };
      };
  in
    flklib.eachSystem (with flklib.system; [x86_64-linux]) (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            fontconfig = with prev;
              fontconfig.overrideAttrs (old: {
                propagatedNativeBuildInputs =
                  (
                    lib.optionals (builtins.hasAttr "propagatedNativeBuildInputs" old)
                    old.propagatedNativeBuildInputs
                  )
                  ++ [
                    zlib.dev
                    bzip2.dev
                    libpng.dev
                    brotli.dev
                  ];
              });
          })
        ];
      };
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
      checks =
        {
          inherit my-crate;
          my-crate-clippy = craneLib.cargoClippy (commonArgs
            // {
              inherit cargoArtifacts;
              cargoClippyExtraArgs = "--all-targets -- --deny warnings";
            });
          my-crate-doc = craneLib.cargoDoc (commonArgs
            // {
              inherit cargoArtifacts;
            });
          my-crate-fmt = craneLib.cargoFmt {
            inherit src;
          };
          # maybe in the far far future this can be uncommented
          # my-crate-audit = craneLib.cargoAudit {
          #   inherit src;
          #   inherit (inputs) advisory-db;
          # };
          my-crate-nextest = craneLib.cargoNextest (commonArgs
            // {
              inherit cargoArtifacts;
              partitions = 1;
              partitionType = "count";
            });
        }
        // lib.optionalAttrs (system == flklib.system.x86_64-linux) {
          my-crate-coverage = craneLib.cargoTarpaulin (commonArgs
            // {
              inherit cargoArtifacts;
            });
        };

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
        cachix-push = with pkgs;
        with lib.strings; let
          script = writeScriptBin "cachix-push" (concatStringsSep "\n" [
            # Push flake inputs: as flake inputs are downloaded from the
            # internet, they can disappear
            ''
              nix flake archive --json \
                | jq -r '.path,(.inputs|to_entries[].value.path)' \
                | ${pkgs.cachix}/bin/cachix push ${cache}
            ''
            # Pushing runtime closure of all packages in a flake:
            ''
              nix build --json \
                | jq -r '.[].outputs | to_entries[].value' \
                | ${pkgs.cachix}/bin/cachix push ${cache}
            ''
            # Pushing shell environment
            ''
              nix develop --profile dev-profile
              ${pkgs.cachix}/bin/cachix push mycache dev-profile
            ''
          ]);
        in {
          type = "app";
          program = "${script}/bin/cachix-push";
        };

        cachix-pull = with pkgs;
        with lib.strings; let
          script = writeScriptBin "cachix-pull" (concatStringsSep "\n" [
            # Optional as we already set substituters above
            # "${pkgs.cachix}/bin/cachix use ${cache}"
            "nix build" # build with cachix
            "nix develop --profile dev-profile --command 'exit 0'" # build dev shell with cachix
            # this last line is important for bootstrapping, especially if you use nix-direnv
          ]);
        in {
          type = "app";
          program = "${script}/bin/cachix-pull";
        };
      };

      devShells.default = devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [
          {
            # git configuration block
            pre-commit.hooks = {
              shellcheck.enable = true;
              clippy.enable = true;
              hunspell.enable = true;
              alejandra.enable = true; # nix formatter
              # statix.enable = true; # lints for nix, but apparently borked
              rustfmt.enable = true;
              typos.enable = true;
            };
          }
          {
            # ad-hoc dev packages
            packages = with pkgs; [
              # dice cli:
              inputs.dice.packages.${system}.default
              # python for the rsdd visualizer
              (python3.withPackages (p:
                with p; [
                  graphviz
                ]))
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
            ];
          }
          rec {
            # rust dev block
            languages.rust.enable = true;

            # add a rust-repl
            scripts.repl.exec = "${pkgs.evcxr}/bin/evcxr";

            # https://devenv.sh/reference/options/
            packages =
              with pkgs;
                [
                  lldb

                  cargo
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

                  # tree-sitter-specific
                  tree-sitter
                ]
                ++ lib.optionals stdenv.isDarwin []
                ++ lib.optionals stdenv.isLinux [
                  cargo-rr
                  rr-unstable
                ]
              # ++ builtins.attrValues self.checks
              ;
            # shell block
            env.DEVSHELL = "devshell+flake.nix";
            enterShell = pkgs.lib.strings.concatStringsSep "\n" ([
                ''
                  echo ""
                  echo "Hello from $DEVSHELL!"
                  echo "Some tools this environment is equipped with:"
                  echo ""
                ''
              ]
              ++ (builtins.map (
                  p: "echo \"${p.pname}\t\t-- ${p.meta.description}\""
                )
                packages)
              ++ ["echo \"\""]);
          }
        ];
      };
    });
}
