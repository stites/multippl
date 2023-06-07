prev: name: _pkg:
prev.rustBuilder.rustLib.makeOverride {
  inherit name;
  overrideAttrs = drv: {
    propagatedBuildInputs =
      drv.propagatedBuildInputs
      ++ (with prev; [
        cmake
        pkg-config
        _pkg
      ]);
    propagatedNativeBuildInputs =
      (
        if builtins.hasAttr drv "propagatedNativeBuildInputs"
        then drv.propagatedNativeBuildInputs
        else []
      )
      ++ (with prev; [
        cmake
        pkg-config
        _pkg
      ]);
  };
}
