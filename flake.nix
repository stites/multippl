{
  nixConfig.trusted-substituters = "https://lean4.cachix.org/";
  nixConfig.trusted-public-keys = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= lean4.cachix.org-1:mawtxSxcaiWE24xCXXgh3qnvlTkyU7evRRnGeAhD4Wk=";
  nixConfig.max-jobs = "auto";  # Allow building multiple derivations in parallel
  nixConfig.keep-outputs = true;  # Do not garbage-collect build time-only dependencies (e.g. clang)

  description = "A project in lean";

  inputs = {
    # devenv requires 22.11 compatability, so we pin nixpkgs to this package set.
    devenv.url = "github:cachix/devenv";
    nixpkgs.follows = "devenv/nixpkgs";

    # next, we include lean, and
    lean.url = "github:leanprover/lean4";
    lean.inputs.nixpkgs.follows = "devenv/nixpkgs";

    # and we re-use lean's flake-utils:
    flake-utils.follows = "lean/flake-utils";

    lean-doc.url = github:leanprover/lean4?dir=doc;
    lean-doc.inputs.lean.follows = "lean";
    lean-doc.inputs.flake-utils.follows = "flake-utils";
    mathlib4.url = "github:leanprover-community/mathlib4";
    mathlib4.flake = false;
    aesop.url = github:JLimperg/aesop;
    aesop.flake = false;
    quote.url = github:gebner/quote4;
    quote.flake = false;
    std4.url = github:leanprover/std4;
    std4.flake = false;

    # Lots of dependencies for this one: https://github.com/leanprover/doc-gen4
    doc-gen4.url = github:leanprover/doc-gen4;
    doc-gen4.flake = false;

    # deps:
    cmark.url = github:xubaiw/CMark.lean;
    cmark.flake = false;
    unicode.url = github:xubaiw/Unicode.lean;
    unicode.flake = false;
    cli.url = github:mhuisi/lean4-cli;
    cli.flake = false;
    lean-ink.url = github:leanprover/LeanInk;
    lean-ink.flake = false;
    lake.url = github:leanprover/lake;
    lake.inputs.lean.follows = "lean";
    lake.inputs.flake-utils.follows = "flake-utils";

    # take the streaming library from:
    kha-aoc-2022.url = github:Kha/aoc-2022;
    kha-aoc-2022.inputs.lean.follows = "lean";
    kha-aoc-2022.inputs.lean-doc.follows = "lean-doc";
    kha-aoc-2022.inputs.flake-utils.follows = "lean/flake-utils";
    kha-aoc-2022.inputs.std4.follows = "std4";
  };

  outputs = inputs@{ self, lean, ... }: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      myPackageName = "BddIS";
      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            inherit (leanPkgs) lean-bin-dev lake-dev emacs-dev;
            vscode-dev = final.vscode-with-extensions.override {
              vscodeExtensions = [ leanPkgs.vscode-lean4 pkgs.vscode-extensions.vscodevim.vim ];
            };
          })
        ];
      };
      leanPkgs = lean.packages.${system};
      lean-doc = inputs.lean-doc.packages.${system};
      myLeanPkg = leanPkgs.buildLeanPackage {
        name = myPackageName;
        src = ./.;
        roots = [ myPackageName ];
        #deps = [ std4 inputs.kha-aoc-2022.defaultPackage ];
        deps = [ std4 ];
        libName = myPackageName;
        # executableName = myPackageName;
      };
      # having some trouble with this one. Going to just inline the definitions
      # kha-aoc-2022 = inputs.kha-aoc-2022.packages.${system}.modRoot.overrideAttrs (old: {
      #   #allExternalDeps = old.allExternalDeps;
      #   allExternalDeps = [];
      # });

      mathlib = leanPkgs.buildLeanPackage {
        name = "Mathlib";
        src = inputs.mathlib4;
        precompilePackage = true;
        deps = [ aesop quote std4
                 # optional doc-gen4
               ];
      };
      aesop = leanPkgs.buildLeanPackage {
        name = "Aesop";
        src = inputs.aesop;
        precompilePackage = true;
        deps = [ std4 ];
      };
      quote = leanPkgs.buildLeanPackage {
        name = "Qq";
        src = inputs.quote;
        precompilePackage = true;
      };
      std4 = leanPkgs.buildLeanPackage {
        name = "Std";
        src = inputs.std4;
        precompilePackage = true;
      };
      # doc-gen4 = leanPkgs.buildLeanPackage {
      #   name = "DocGen4";
      #   src = inputs.std4;
      #   precompilePackage = true;
      #   deps = [ cmark unicode cli inputs.lake.defaultPackage lean-ink ];
      # };
      # cmark = leanPkgs.buildLeanPackage {
      #   name = "CMark";
      #   src = inputs.cmark;
      #   precompilePackage = true;
      #   deps = [ pkgs.cmark ];
      # };
      # unicode = leanPkgs.buildLeanPackage {
      #   name = "Unicode";
      #   src = inputs.unicode;
      #   precompilePackage = true;
      # };
      # cli = leanPkgs.buildLeanPackage {
      #   name = "Cli";
      #   src = inputs.cli;
      #   precompilePackage = true;
      # };
      # lean-ink = leanPkgs.buildLeanPackage {
      #   name = "LeanInk";
      #   src = inputs.lean-ink;
      #   precompilePackage = true;
      # };
      # renders = let
      #   mods = lean-doc.renderDir "Do" ./Do;
      #   mods' = map (drv: drv.overrideAttrs (_: { LEAN_PATH = pkg.modRoot; })) mods.paths;
      # in symlinkJoin { name = "Fp"; paths = mods'; };
      # book = pkgs.stdenv.mkDerivation {
      #   name = "do-doc";
      #   src = ./doc;
      #   buildInputs = [ lean-doc.lean-mdbook ];
      #   buildPhase = ''
      #     mkdir $out
      #     # necessary for `additional-css`...?
      #     cp -r --no-preserve=mode ${inputs.lean-doc}/doc/*.{js,css} .
      #     cp -r ${renders}/* .
      #     mdbook build -d $out
      # '';
      #   dontInstall = true;
      # };
    in {
      packages = myLeanPkg // {
        inherit (leanPkgs) lean;
        default = myLeanPkg.modRoot;
      };

      devShells.default = inputs.devenv.lib.mkShell {
        inherit inputs pkgs;
        modules = [ {
          # set up some nice, out-of-the-box dev support:
          languages.nix.enable = true;
          difftastic.enable = true; # https://devenv.sh/integrations/difftastic/
          pre-commit.hooks.shellcheck.enable = true;

          packages = with pkgs; [  lldb rr-unstable lean-bin-dev lake-dev emacs-dev ]; # leanPkgs.lean
          env.GREET = "devenv";
          # taken from Mathlib, but not really applicable for most use-cases
          scripts = {
            hello.exec = "echo hello from $GREET";
            code.exec = "${pkgs.vscode-dev}/bin/vscode-dev";
            # Add all new *.lean files to ${myPackageName}.lean
            mk-lib-root.exec = ''
              cd $(git rev-parse --show-toplevel)
              find . -name '*.lean' -not -name '${myPackageName}.lean' | env LC_ALL=C sort | cut -d '/' -f 2- | sed 's/\\.lean//;s,/,.,g;s/^/import /' > ${myPackageName}.lean
            '';
          };
          enterShell = "hello";
        } ];
      };
    });
}
