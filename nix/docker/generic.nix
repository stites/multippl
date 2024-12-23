{
  name,
  config ? {},
  dockerTools,
  buildEnv,
  psi,
  python-with-pyro,
  multippl,
  dice,
  benchmark-cli,
  bash,
  coreutils,
}:
dockerTools.buildImage {
  inherit name;
  tag = "latest";

  copyToRoot = buildEnv {
    name = "image-root";
    paths = [psi python-with-pyro multippl dice benchmark-cli bash coreutils];
    pathsToLink = ["/bin"];
  };
  config =
    {
      WorkingDir = "/data";
    }
    // config;
}
