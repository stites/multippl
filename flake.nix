{
  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    dice.url = "github:stites/dice.nix";
    # rsdd.url = "github:stites/rsdd/yodel-additions?dir=nix";
    rsdd.url = "path:/home/stites/git/rust/rsdd/nix";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    ...
  } @ inputs: let
    flklib = inputs.flake-utils.lib;
  in
    flklib.eachSystem (with flklib.system; [x86_64-linux]) (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
        ];
      };
      craneLib = (inputs.crane.mkLib pkgs).overrideScope' (final: prev: let
        mk-sys-package = name: _pkg:
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
      in {
        # for plotters
        expat-sys = mk-sys-package "expat-sys" prev.expat;
        freetype-sys = mk-sys-package "freetype-sys" prev.freetype;
        fontconfig-sys = mk-sys-package "fontconfig-sys" prev.fontconfig;
      });

      my-crate = craneLib.buildPackage {
        src = craneLib.cleanCargoSource ./.;
        buildInputs =
          [
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            # Additional darwin specific inputs can be set here
            pkgs.libiconv
          ];
      };
    in {
      checks = {
        inherit my-crate;
      };

      packages.default = my-crate;

      apps.default = inputs.flake-utils.lib.mkApp {
        drv = my-crate;
      };

      devShell = devenv.lib.mkShell {
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
            # ad-hoc dev stuff for new graphviz cli in rsdd
            packages = with pkgs; [
              (python3.withPackages (p:
                with p; [
                  graphviz
                ]))
            ];
          }
          {
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
                  cargo-play # quickly run a rust file that has a maint function

                  # for plotters / servo-fontconfig-sys. Helps to symlink /etc/profiles/per-user/$USER/bin/file to /usr/bin/file
                  cmake
                  pkg-config
                  freetype
                  expat
                  fontconfig
                ]
                ++ lib.optionals stdenv.isDarwin []
                ++ lib.optionals stdenv.isLinux [
                  cargo-rr
                  rr-unstable
                ]
              # ++ builtins.attrValues self.checks
              ;
          }
          {
            # tree-sitter specific block
            packages = with pkgs; [tree-sitter];
          }
          {
            packages = [inputs.dice.packages.${system}.default];
          }
          {
            # shell block
            env.DEVSHELL = "devshell+flake.nix";
            enterShell = ''
              echo "hello from $DEVSHELL!"
            '';
          }
        ];
      };
    });
}
