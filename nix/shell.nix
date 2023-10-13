{
  inputs,
  pkgs,
  lib,
}:
inputs.devenv.lib.mkShell {
  inherit inputs pkgs;
  modules = [
    {
      # git configuration block
    }
    {
      # ad-hoc dev packages
      packages = with pkgs; [
        # dice cli:
        inputs.dice.packages.${system}.default
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
        cmdstan
      ];
    }
    rec {
      # rust dev block
      languages.rust.enable = true;
      languages.nix.enable = true;

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

            cargo-tarpaulin # code coverage

            # tree-sitter-specific
            tree-sitter

            # polish tools: from https://ectobit.com/blog/speed-up-github-actions-rust-pipelines/
            cargo-deny # lint dependencies
            cargo-outdated # find out outdated dependencies
            cargo-udeps # find out unused dependencies
            cargo-audit # search for security vulnerabilities
            # cargo-pants # search for security vulnerabilities (by sonatype )
          ]
          ++ lib.optionals stdenv.isDarwin []
          ++ lib.optionals stdenv.isLinux [
            cargo-rr
            rr-unstable
          ]
          ++ [
            cmdstan
            (rWrapper.override {
              packages = with pkgs.rPackages; [
                rstan
                bnlearn
                ggplot2
                tidyverse
              ];
            })
            # python for the rsdd visualizer
            (python3.withPackages (p:
              with p; [
                graphviz
                matplotlib
                seaborn
                numpy
                pandas
                # pyro-ppl
                # ] ++  pyro-ppl.optional-dependencies.extras ))
              ]))
          ]
        # ++ builtins.attrValues self.checks
        ;
      # shell block
      env.DEVSHELL = "devshell+flake.nix";
      enterShell = pkgs.lib.strings.concatStringsSep "\n" [
        ''echo "Hello from $DEVSHELL!"''
        (inputs.nixlib.lib.my.menu {inherit packages;})
        ''echo ""''
      ];
    }
  ];
}
