{
  config,
  pkgs,
  ...
}: let
  input-pkgs = {
    multippl = config.packages.default;
    psi = config.packages.psi;
    python-with-pyro = config.packages.pyro;
  };
in {
  repl = pkgs.callPackage ./generic.nix (input-pkgs
    // {
      name = "multippl-repl";
      config.Cmd = "${pkgs.bashInteractive}/bin/bash";
    });

  bin = pkgs.callPackage ./generic.nix (input-pkgs
    // {
      name = "multippl-bin";
      config.Cmd = "${config.packages.default}/bin/yodel";
    });

  bench = pkgs.callPackage ./generic.nix (input-pkgs
    // {
      name = "multippl-bench";
      config.Cmd = "${config.packages.benchmark-cli}/bin/multippl-benchmark";
    });
}
