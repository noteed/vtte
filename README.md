# Kilo, wrapped in Haskell

This is [antirez's kilo](https://github.com/antirez/kilo) text editor (written
in C), wrapped in Haskell. No idea where this is going.

The original `kilo.c` file is in `cbits/`, as often done with Haskell projects
relying on C code.

Building the code can be done with `nix-build release.nix`.
