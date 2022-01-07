let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  inherit (import sources."gitignore.nix" { inherit (pkgs) lib; })
    gitignoreSource;

  # I don't think we can use the idris build command because it uses the idris 1
  # executable. However, there may be a way to modify that command to force it
  # to use idris 2.
in pkgs.stdenv.mkDerivation {
  name = "advent-of-code-2021";
  buildInputs = [ pkgs.idris2 ];

  src = gitignoreSource
    (pkgs.lib.sourceByRegex ./. [ "^src.*" "^advent-of-code.ipkg$" ]);

  buildPhase = ''
    idris2 --build advent-of-code.ipkg
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp -r build/exec/* $out/bin/
  '';
}
