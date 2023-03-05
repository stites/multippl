# quick stub from https://nixos.org/guides/building-and-running-docker-images.html
# for creating docker images from nix. turn all flake.nix#apps into runnable docker images.
# don't forget to:
#
# $ docker load < result
# $ docker run -t hello-docker:y74sb4nrhxr975xs7h83izgm8z75x5fc
#
# to validate that it all works
{
  pkgs ? import <nixpkgs> {},
  pkgsLinux ? import <nixpkgs> {system = "x86_64-linux";},
}:
pkgs.dockerTools.buildImage {
  name = "hello-docker";
  config = {
    Cmd = ["${pkgsLinux.hello}/bin/hello"];
  };
}
