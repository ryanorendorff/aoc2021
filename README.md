Advent of Code 2021 -- In Idris 2!
==================================

[![Builds and Runs](https://github.com/ryanorendorff/aoc2021/workflows/Builds%20and%20Runs/badge.svg)][builds-and-runs]

This is my attempt to learn a new language through a set of problems. I was
mostly attempting to learn Idris 2 and how to set it up through Nix over
answering the prompts.

To build the package, run

```
nix-build release.nix
```

and then the executable should be able to be run through `./result/bin/aoc`.
Note that this executable can be run from anywhere; the inputs are copied to the
nix store to make sure that the executable is deterministically run.
