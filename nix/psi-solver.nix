{
  lib,
  ldc,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  name = "psi-solver-2023-04-04"; # stable as of 2023-12-05
  version = "stable";
  src = fetchFromGitHub {
    owner = "eth-sri";
    repo = "psi";
    rev = "1e9f1b2c27bf8071188f3449e29121e43d25ca25";
    fetchSubmodules = true;
    sha256 = "0hk9r836cfnygwiwd4gq7m8y1p92vs6ppd6nipjcfxy4hxd8kk9q";
  };
  buildInputs = [ldc];
  buildPhase = ''
    ldmd2 -v -O -release -inline -boundscheck=off -J. -Jlibrary *.d ast/*.d sym/*.d util/*.d -ofpsi
  '';
  doCheck = true;
  checkPhase = ''
    ldmd2 -v test/runtests.d -oftest/runtests
    (cd test && ./runtests)
  '';
  installPhase = let
  in ''
    mkdir -p $out/bin
    cp psi $out/bin/psi
  '';
  meta = with lib; {
    homepage = "https://github.com/eth-sri/psi";
    description = "Exact Inference Engine for Probabilistic Programs";
    longDescription = ''
      PSI is a tool for performing exact inference on probabilistic programs.
      Given a probabilistic program, the tool produces an expression for the
      joint posterior distribution of the expressions computed by the program.
    '';
    platforms = platforms.all;
    maintainers = [maintainers.stites];
  };
}
