{ pythonVersion ? "python39" }:
let
  sources = {
    haskellNix = builtins.fetchTarball {
      name = "haskell-nix-snap";
      url = "https://github.com/input-output-hk/haskell.nix/archive/c689f01730e5b6c6c16d3947a15689569844c38c.tar.gz";
      sha256 = "09lw2419a5dd9g0ja31hjfqf6d4bzcgr5mrqx0vrvlksmp7a1kzk";
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

