# Haskell

## Tools

### ghcid

[ghcid](https://github.com/ndmitchell/ghcid) is a `Very low feature GHCi based IDE`.
It can be used to re-run a function in a given file on changes in a given directory.
This template provides a sample configuration for this tool in the `.ghcid` file.

## Nix

- [haskell4nix](https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html)
- [Incrementally package a Haskell program](https://www.haskellforall.com/2022/08/incrementally-package-haskell-program.html)
- [Nixpkgs support for incremental Haskell builds](https://www.haskellforall.com/2022/12/nixpkgs-support-for-incremental-haskell.html)

## package.yaml

It is translated via [hpack](https://github.com/sol/hpack) into [.cabal](https://cabal.readthedocs.io/en/3.8/cabal-package.html).
`hpack` helps to avoid repetitions in `.cabal`.
