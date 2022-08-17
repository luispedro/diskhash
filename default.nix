{ pythonVersion ? "python39" }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/17e5f35d56c57b20ba2397010fcd4032fb6acc2b.tar.gz";
      sha256 = "0b3pwrib289bhidv1q27k6k53zwyahdffi2ynww19qnviqwlm21a";
    };
  };

  haskellNix = import sources.haskellNix { };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

    ignoredPaths = [".github"];
    src = pkgs.lib.cleanSourceWith {
      # 'cleanGit' cleans a source directory based on the files known by git
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "diskhash";
        src = ./.;
      };
      # ignore paths that change frequently, but do not contribute to the result
      filter = path: type: let baseName = baseNameOf (toString path); in !(pkgs.lib.elem baseName ignoredPaths);
    };
  pythonPackages = pkgs.${pythonVersion + "Packages"};
in {
  haskell = pkgs.haskell-nix.stackProject { inherit src; };
  python = pythonPackages.buildPythonPackage {
    name = "diskshash";
    inherit src;

    checkInputs = [
      pythonPackages.pytest
      pythonPackages.hypothesis
    ];
    checkPhase = ''
      cp -pir python/diskhash/tests $TMPDIR/tests_tmp
      ${pythonPackages.python.interpreter} -m pytest $TMPDIR/tests_tmp
    '';
    propagatedBuildInputs = [
      pythonPackages.python
      pkgs.zlib
    ];
 };

}

