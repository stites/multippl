{
  stdenv,
  bench_psi ? false,
  psi,
  pyro,
  multippl,
  config,
  bash,
  bashInteractive,
  ghc,
  bc,
  dice,
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
    name = "multippl-benchmark-cli";
    version = "HEAD";
    src = ../../.;

    buildInputs = [
      psi
      pyro
      multippl
      cabal
      bc
      dice
    ];

    buildPhase = let
      hotwire-binaries = file: ''
        substituteInPlace ${file} \
          --replace "cmd = [\"python" "cmd = [\"${pyro}/bin/python" \
          --replace "timedrunner(\"psi" "timedrunner(\"${psi}/bin/psi" \
          --replace "shutil.which(\"yodel\")" "\"${multippl}/bin/yodel\""
      '';
    in ''
      STARTDIR=$PWD
      cd yodel/bench/gossip
      CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal user-config init
      CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal user-config update --augment='offline: True'
      CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal user-config update --augment='active-repositories: :none'
      sed -i '/hackage/d' $PWD/.cabal/config
      CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal build --offline ./truth.hs
      cp $(CABAL_DIR=$PWD/.cabal ${cabal}/bin/cabal list-bin truth.hs) ./truth
      for ex in g4 g10 g20; do
        cd $ex
        bash ../truth.sh
        cd ..
      done
      cd ../bayesnets
      for ex in alarm insurance; do
        cd $ex
        substituteInPlace ./truth.sh --replace "  dice" "  ${dice}/bin/dice"
        bash ./truth.sh
        cd ..
      done
      cd $STARTDIR

      mkdir -p $out/bin
      mkdir -p $out/bench/bayesnets
      cp {yodel,$out}/bench/avg.py
      cp {yodel,$out}/bench/bench.py
      substituteInPlace $out/bench/bench.py \
          --replace "cmd = [\"python" "cmd = [\"${pyro}/bin/python" \
          --replace "timedrunner(\"psi" "timedrunner(\"${psi}/bin/psi" \
          --replace "timedrunner(\"dice" "timedrunner(\"${dice}/bin/dice" \
          --replace "shutil.which(\"yodel\")" "\"${multippl}/bin/yodel\""
      substituteInPlace yodel/bench/arrival/run.sh --replace "python" "${pyro}/bin/python"
      cp {yodel,$out}/bench/runall.sh

      cp {yodel,$out}/bench/stdin2l1.py
      cp {yodel,$out}/bench/util.py

      for ex in gossip arrival bayesnets/alarm bayesnets/run.sh bayesnets/insurance grids; do
        cp -r yodel/bench/$ex $out/bench/$ex
      done

      cat <<EOF > $out/bin/multippl-benchmark
      #!${bashInteractive}/bin/bash
      WORKING_DIR=\$(pwd)
      for exp in "grids" "arrival" "gossip" "bayesnets"; do
          cd $out/bench/\$exp
          if bash ./run.sh "\$@" --logdir \$WORKING_DIR/logs/\$exp; then continue; else exit 1; fi
      done
      EOF
      chmod a+x $out/bin/multippl-benchmark
    '';
  }
