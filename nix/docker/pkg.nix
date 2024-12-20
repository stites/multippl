{
  dockerTools,
  buildEnv,
  hello,
}:
dockerTools.buildImage {
  name = "hello";
  tag = "latest";

  copyToRoot = buildEnv {
    name = "image-root";
    paths = [hello];
    pathsToLink = ["/bin"];
  };

  extraCommands = ''
    mkdir -p data
    echo "some content" > my-file
  '';

  config = {
    Cmd = ["/bin/hello"];
    WorkingDir = "/data";
  };
}
