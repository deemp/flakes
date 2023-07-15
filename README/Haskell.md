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
- [haskell-nix](https://github.com/Gabriella439/haskell-nix) - examples of packaging haskell apps
- [nixpkgs](https://github.com/NixOS/nixpkgs/blob/ea692c2ad1afd6384e171eabef4f0887d2b882d3/pkgs/development/haskell-modules/hackage-packages.nix) - Haskell packages
- [Fixing broken Haskell packages in Nixpkgs](https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/)

### completions

```nix
devShells.default = pkgs.mkShell {
    buildInputs = [ packageExe ];
    shellHook = ''
      source ${packageExe}/share/bash-completion/completions/${packageExecutableName}
    '';
  };
```

## package.yaml

It is translated via [hpack](https://github.com/sol/hpack) into [.cabal](https://cabal.readthedocs.io/en/3.8/cabal-package.html).
`hpack` helps to avoid repetitions in `.cabal`.
