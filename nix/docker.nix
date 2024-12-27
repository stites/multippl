{
  config,
  pkgs,
  dockerTools,
  buildEnv,
  ...
}: let
  multippl = config.packages.default;
  multippl-source = config.packages.multippl-source;
  multippl-benchmark = config.packages.multippl-benchmark;
  psi = config.packages.psi;
  python-with-pyro = config.packages.pyro;
  dice = config.packages.dice;
in
  dockerTools.buildImage {
    name = "multippl";
    tag = "latest";

    copyToRoot = buildEnv {
      name = "image-root";
      paths =
        [
          psi
          python-with-pyro
          multippl
          dice
          multippl-benchmark
          multippl-source
        ]
        ++ (with pkgs; [
          # basic bash repl
          bash
          zsh
          iputils
          iproute2
          coreutils
          ncurses
          openssl

          # dev dependencies
          tree-sitter
          git
          cargo
          clippy
          rustc
          rustfmt
          rust-analyzer
          clippy
          cargo-watch
          cargo-nextest
          bacon

          # extra dev dependencies for benchmark groundtruth generation
          bc
          ghc
        ]);
      pathsToLink = ["/bin"];
    };
    runAsRoot = ''
      #!${pkgs.runtimeShell}
      mkdir -p /data/logs
      cp -r ${multippl-source}/local/share/multippl-source /data/multippl-source
      chmod -R a+rw /data/multippl-source
      cat <<EOF > /data/docker-entrypoint.sh
      #!/bin/bash
      echo \$PWD
      git config --global http.sslVerify false
      cd /data/multippl-source
      git init
      export CARGO_NET_GIT_FETCH_WITH_CLI=true
      echo cd \$PWD
      echo "WARNING: using docker is not supported for development, as the compiled OpenSSL version is out of date."
      echo "Please use the nix development shell if you are interested in a preconfigured development environment,"
      echo "or file an issue on GitHub."
      EOF
    '';
    config = {
      WorkingDir = "/data";
      Entrypoint = ["${config.packages.multippl}/bin/multippl"];
    };
  }
