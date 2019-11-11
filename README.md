# Kilo, wrapped in Haskell

This is [antirez's kilo](https://github.com/antirez/kilo) text editor (written
in C), wrapped in Haskell. No idea where this is going.

The original `kilo.c` file is in `cbits/`, as often done with Haskell projects
relying on C code.

Building the code can be done with `nix-build release.nix`, or, if you don't
want to explicitely clone this repository:

```
$ nix-build https://github.com/noteed/vtte/archive/master.tar.gz -A vtte
```
