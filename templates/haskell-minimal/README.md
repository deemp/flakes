# haskell-minimal

This is a minimal flake for developing local `Haskell` packages.

It composes a [devshell](https://github.com/numtide/devshell) and a [shellFor](https://nixos.wiki/wiki/Haskell#Using_shellFor_.28multiple_packages.29).

## This flake

The following tools are available in the `devShell`:

- `cabal-install`
- `hpack`
- the `GHC` compiler
- the `Haskell Language Server`
- dependencies of local packages
