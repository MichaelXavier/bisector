{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc884"
}:

#TODO: niv
let
  nix = import ./nix;

  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

  inherit (pkgs) lib;

  baseHaskellPkgs = pkgs.haskell.packages.${ghc};

  # Haskell package set overlay containing overrides for newer packages than
  # are included in the base nixpkgs set as well as additions from vendored
  # or external source repositories.
  haskellPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix { inherit (nix) sources; };

  bisectorOverlay = pkgs.callPackage ./nix/haskell/bisector.nix { inherit (nix) sources; };

   # Construct the final Haskell package set
  haskellPkgs = baseHaskellPkgs.override (
    old: {
      overrides = builtins.foldl' pkgs.lib.composeExtensions
        (old.overrides or (_: _: {})) [
          haskellPkgSetOverlay
          bisectorOverlay
        ];
    }
  );

  shell = haskellPkgs.shellFor {
    packages = p: [
      p.bisector
    ];

    withHoogle = true;

    # include any recommended tools
    nativeBuildInputs = [
      haskellPkgs.ghcid
      haskellPkgs.hpack
      haskellPkgs.cabal-install
    ];

    # Ensure that the cabal file is present for cabal package's flags
    # are respected by configuring. We add on a flag to disable writing
    # of environment files which occasionally result in dependency
    # issues.
    shellHook = ''
      # temporarily abort on errors
      set -e
      hpack package.yaml
      # turn abort on errors off
      set +e
      '';
  };

in {
  inherit shell;
}
