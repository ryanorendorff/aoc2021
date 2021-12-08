let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

in pkgs.stdenv.mkDerivation {
  name = "advent-of-code-2021";
  buildInputs = [ pkgs.idris2 ];
  src = ./src;
}
