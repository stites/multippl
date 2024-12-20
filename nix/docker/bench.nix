{
  name,
  config ? {},
  dockerTools,
  buildEnv,
  psi,
  python-with-pyro,
  multippl,
}:
dockerTools.buildImage {
  inherit name;
  tag = "latest";

  copyToRoot = buildEnv {
    name = "image-root";
    paths = [psi python-with-pyro multippl];
    pathsToLink = ["/bin"];
  };

  config =
    {
      WorkingDir = "/data";
    }
    // config;
}
