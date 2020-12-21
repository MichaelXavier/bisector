{ haskell
, lib
, sources
}:

hfinal: hprev:

let
  bisector =
    let
      inherit (hfinal) callCabal2nix;
      src = lib.sourceByRegex ../.. [
        "Main.hs"
        "package.yaml"
      ];
    in callCabal2nix "bisector" src {};

in
{
  inherit bisector;
}
