{
  stdenv,
  cargoDevArtifacts,
  gnutar,
  zstd,
  ...
}:
stdenv.mkDerivation {
  name = "multippl-source";
  version = "HEAD";
  src = ../.;

  buildInputs = [zstd];
  buildPhase = ''
    mkdir -p $out/local/share
    cp -r $src $out/local/share/multippl-source
  '';
}
