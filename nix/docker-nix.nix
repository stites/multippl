{
  pkgs,
  dockerTools,
  buildEnv,
  archSuffix,
  psi,
  python-with-pyro,
  multippl,
  dice,
  multippl-benchmark,
  multippl-source,
  ...
}:
dockerTools.buildNixShellImage {
  name = "multippl-nix";
  tag = "latest-${archSuffix}";

  drv = pkgs.hello.overrideAttrs (old: {
    nativeBuildInputs =
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
        coreutils-full
        ncurses
        openssl
        nix
        vim

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
    shellHook = ''
      mkdir -p /build/logs
      cp -r ${multippl-source}/local/share/multippl-source /build/multippl-source
      cp -r ${multippl-source}/local/share/multippl-source/examples /build/examples
      chmod -R a+rw /build/multippl-source
      cat <<EOF > /build/docker-entrypoint.sh
      #!/bin/bash
      echo \$PWD
      git config --global http.sslVerify false
      cd /build/multippl-source
      git init
      export CARGO_NET_GIT_FETCH_WITH_CLI=true
      echo cd \$PWD
      echo "WARNING: using docker is not supported for development, as the compiled OpenSSL version is out of date."
      echo "Please use the nix development shell if you are interested in a preconfigured development environment,"
      echo "or file an issue on GitHub."
      EOF
    '';
  });
  homeDirectory = "/build";
}
