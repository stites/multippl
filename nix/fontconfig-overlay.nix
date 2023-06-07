final: prev: {
  fontconfig = with prev;
    fontconfig.overrideAttrs (old: {
      propagatedNativeBuildInputs =
        (
          lib.optionals (builtins.hasAttr "propagatedNativeBuildInputs" old)
          old.propagatedNativeBuildInputs
        )
        ++ [
          zlib.dev
          bzip2.dev
          libpng.dev
          brotli.dev
        ];
    });
}
