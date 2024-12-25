{
  stdenv,
  bench_psi ? false,
  psi,
  pyro,
  multippl,
  config,
  bash,
  ghc,
  bc,
  ...
}: let
  cabal = ghc.withPackages (ps: [ps.cabal-install ps.containers]);
  erun = cmd: ''
    echo ${cmd}
    sleep 1
    ${cmd}
  '';
in
  stdenv.mkDerivation {
    name = "multippl-benchmark";
    version = "HEAD";
    src = ../.;

    buildInputs = [
      psi
      pyro
      multippl
      cabal
      bc
    ];

    buildPhase = let
      go = name:
        builtins.concatStringsSep "\n" (map erun [
          "cd bench/${name}"
          "${bash}/bin/bash $(pwd)/run.sh bench ${
            if bench_psi
            then "--psi"
            else "--no-psi"
          }"
          "cd -"
        ]);
    in
      builtins.concatStringsSep "\n" ((map go [
          "arrival"
          "grids"
          "bayesnets"
        ])
        ++ (map erun [
          "cd bench/gossip"
          "CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal user-config init"
          "CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal user-config update --augment='offline: True'"
          "CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal user-config update --augment='active-repositories: :none'"
          "sed -i '/hackage/d' $PWD/.cabal/config"
          "CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal build --offline truth.hs"
          "cp $(CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal list-bin truth.hs) truth"
          "cd -"
          "cd bench/gossip/g4"
          "${bash}/bin/bash ./truth.sh"
          "cd -"
          "cd bench/gossip/g10"
          "${bash}/bin/bash ./truth.sh"
          "cd -"
          "cd bench/gossip/g20"
          "${bash}/bin/bash ./truth.sh"
          "cd -"
          (go "gossip")
          "mkdir -p $out/logs/g{4,10,20}"
          "mv bench/gossip/g20/logs $out/logs/g20"
          "mv bench/gossip/g10/logs $out/logs/g10"
          "mv bench/gossip/g4/logs  $out/logs/g4"
          "ls $out/logs/g4"
        ]));
  }
