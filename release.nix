# As a release, we bundle in the input sources and force the executable to run
# from the directory containing those inputs, so that it always runs.
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  inputs = pkgs.linkFarm "aoc-inputs" [{
    name = "inputs";
    path = ./inputs;
  }];

  aoc = import ./default.nix;

in pkgs.writeShellScriptBin "aoc" ''
  ${pkgs.coreutils}/bin/env -C ${inputs} ${aoc}/bin/aoc
''
