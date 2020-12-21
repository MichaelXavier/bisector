let
  sources = import ./sources.nix;
in

rec {
  inherit sources;

  # A pinned version of nixpkgs, widely used and hopefully well cached.
  defaultNixpkgs = import sources.nixpkgs;

  # A package set for the specified system, based on `defaultNixpkgs`, with
  # additional arguments and all overlays applied.
  pkgSetForSystem =
    system: args: defaultNixpkgs (args // { inherit system; });

  # `pkgSetForSystem` for the current system.
  pkgSet = args: pkgSetForSystem builtins.currentSystem args;

}
