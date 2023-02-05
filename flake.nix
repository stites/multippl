{
  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    ...
  } @ inputs: let
    flklib = inputs.flake-utils.lib;
  in
    flklib.eachSystem (with flklib.system; [x86_64-linux aarch64-darwin]) (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      craneLib = inputs.crane.lib.${system};
      my-crate = craneLib.buildPackage {
        src = craneLib.cleanCargoSource ./.;
        buildInputs =
          [
            # Add additional build inputs here
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
            };
          }
          {
            # rust dev block
            languages.rust.enable = true;

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
