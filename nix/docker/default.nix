{
  config,
  pkgs,
  ...
}: let
  input-pkgs = {
    multippl = config.packages.default;
    psi = config.packages.psi;
    python-with-pyro = config.packages.pyro;
    benchmark-cli = config.packages.benchmark-cli;
    dice = config.packages.dice;
  };
in {
  repl = pkgs.callPackage ./generic.nix (input-pkgs
    // {
      name = "multippl-repl";
      config.Entrypoint = "${pkgs.bashInteractive}/bin/bash";
    });

  bin = pkgs.callPackage ./generic.nix (input-pkgs
    // {
      name = "multippl-bin";
      config.Entrypoint = "${config.packages.default}/bin/yodel";
    });

  bench = pkgs.callPackage ./generic.nix (input-pkgs
    // {
      name = "multippl-bench";
      config.Entrypoint = ["${config.packages.benchmark-cli}/bin/multippl-benchmark"];
    });
}
