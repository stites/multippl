{
  pkgs,
  psi,
  pyro,
  multippl,
  config,
  ghc,
  ...
}: {
  all.psi = pkgs.callPackage ./all.nix {
    inherit psi pyro multippl config;
    bench_psi = true;
  };
  all.no-psi = pkgs.callPackage ./all.nix {
    inherit psi pyro multippl config;
    bench_psi = false;
  };
  cli = pkgs.callPackage ./cli.nix {inherit psi pyro multippl config;};
}
