{
  stdenv,
  psi,
  pyro,
  multippl,
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
    src = ../.;

    buildInputs = [
      psi
      pyro
      multippl
      cabal
      bc
      dice
    ];

    buildPhase = ''
      STARTDIR=$PWD
      cd bench/gossip
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
      cp {,$out/}bench/avg.py
      cp {,$out/}bench/bench.py
      cp {,$out/}bench/tabulate.py
      substituteInPlace $out/bench/bench.py \
          --replace "cmd = [\"python" "cmd = [\"${pyro}/bin/python" \
          --replace "timedrunner(\"psi" "timedrunner(\"${psi}/bin/psi" \
          --replace "timedrunner(\"dice" "timedrunner(\"${dice}/bin/dice" \
          --replace "shutil.which(\"multippl\")" "\"${multippl}/bin/multippl\""
      substituteInPlace bench/arrival/run.sh --replace "python" "${pyro}/bin/python"
      cp {,$out/}bench/multippl-benchmark.sh

      cp {,$out/}bench/stdin2l1.py
      cp {,$out/}bench/util.py

      for ex in gossip arrival bayesnets/alarm bayesnets/run.sh bayesnets/insurance grids; do
        cp -r bench/$ex $out/bench/$ex
      done

      substituteInPlace bench/multippl-benchmark.sh --replace "python" "${pyro}/bin/python" \
                                        --replace "bench/" "$out/bench/"

      cp bench/multippl-benchmark.sh $out/bin/multippl-benchmark
      chmod a+x $out/bin/multippl-benchmark
    '';
  }
