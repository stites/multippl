{
  lib,
  system,
  my-crate,
  craneLib,
  commonArgs,
  cargoArtifacts,
}:
{
  inherit my-crate;

  my-crate-clippy = craneLib.cargoClippy (commonArgs
    // {
      inherit cargoArtifacts;
      cargoClippyExtraArgs = "--all-targets -- --deny warnings -D clippy::redundant-closure-call";
    });

  my-crate-doc = craneLib.cargoDoc (commonArgs
    // {
      inherit cargoArtifacts;
    });

  my-crate-fmt = craneLib.cargoFmt {
    src = ../.;
  };

  # maybe in the far far future this can be uncommented
  # my-crate-audit = craneLib.cargoAudit {
  #   inherit src;
  #   inherit (inputs) advisory-db;
  # };

  my-crate-nextest = craneLib.cargoNextest (commonArgs
    // {
      inherit cargoArtifacts;
      partitions = 1;
      partitionType = "count";
    });
}
// lib.optionalAttrs (system == "x86_64-linux") {
  # don't really need code coverage at this moment
  # my-crate-coverage = craneLib.cargoTarpaulin (commonArgs
  #   // {
  #     cargoTarpaulinExtraArgs = "--skip-clean --out xml --output-dir $out -t 5000";
  #     inherit cargoArtifacts;
  #   });
}
