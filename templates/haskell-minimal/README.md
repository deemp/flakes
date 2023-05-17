# haskell-minimal

This is a minimal flake for developing local `Haskell` packages.

## Problem

Compose a [shellFor](https://nixos.wiki/wiki/Haskell#Using_shellFor_.28multiple_packages.29) and [devshell](https://github.com/numtide/devshell) when using [direnv](https://direnv.net/).

## Solution

Don't use `shellFor`, just get `cabal-install` and `GHC` with necessary packages in a `devShell`.

## This flake

The following tools are available in the `devShell`:

- `cabal-install`
- `hpack`
- the `GHC` compiler
- the `Haskell Language Server`
- dependencies of local packages
