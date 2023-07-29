# Haskell

## Templates

Templates provide extensive information about setting up development environment and packaging Haskell apps.

- [haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme)
- [haskell-simple](https://github.com/deemp/flakes/tree/main/templates/codium/haskell-simple#readme)
- [haskell-minimal](https://github.com/deemp/flakes/tree/main/templates/haskell-minimal#readme)

## Tools

### ghcid

[ghcid](https://github.com/ndmitchell/ghcid) is a `Very low feature GHCi based IDE`.
It can be used to re-run a function in a given file on changes in a given directory.
This template provides a sample configuration for this tool in the `.ghcid` file.

### hpack

[hpack](https://github.com/sol/hpack) translates `package.yaml` into [.cabal](https://cabal.readthedocs.io/en/3.8/cabal-package.html).

## Nix

### haskell4nix

- [haskell4nix](https://haskell4nix.readthedocs.io/nixpkgs-users-guide.html)
- [nixpkgs manual](https://nixos.org/manual/nixpkgs/stable/#haskell)
- [Incrementally package a Haskell program](https://www.haskellforall.com/2022/08/incrementally-package-haskell-program.html)
- [haskell-nix](https://github.com/Gabriella439/haskell-nix) - examples of packaging haskell apps
- [Nixpkgs support for incremental Haskell builds](https://www.haskellforall.com/2022/12/nixpkgs-support-for-incremental-haskell.html)
- Hackage packages in [nixpkgs](https://github.com/NixOS/nixpkgs/blob/ea692c2ad1afd6384e171eabef4f0887d2b882d3/pkgs/development/haskell-modules/hackage-packages.nix) - Haskell packages
- [Fixing broken Haskell packages in Nixpkgs](https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/)
- [Horizon Haskell](https://horizon-haskell.net/)

### haskell.nix

- [haskell.nix](https://input-output-hk.github.io/haskell.nix/)
- [tutorial](https://github.com/Gabriella439/haskell-nix)
- projects: [discord-emoji-dl](https://github.com/deemp/discord-emoji-dl)

### Stack

Uses [haskell4nix](#haskell4nix).

Stack support [Nix integration](https://docs.haskellstack.org/en/stable/nix_integration).

Stack uses [nix-shell](https://nixos.org/manual/nix/unstable/command-ref/nix-shell.html).

- [docs](https://docs.haskellstack.org/en/stable/nix_integration/#nix-integration).

The name of the file providing a Nix shell can be customized in `stack.yaml`.

- [docs](https://docs.haskellstack.org/en/stable/nix_integration/?query=resolver#external-c-libraries-through-a-shellnix-file)
- [example](https://github.com/deemp/flakes/blob/ba6ba57a3c52b8b71263f6b2baf874aaf9e7b631/templates/codium/haskell/stack.yaml#L24)

Here's the format of that file - [docs](https://docs.haskellstack.org/en/stable/nix_integration#supporting-both-nix-and-non-nix-developers).
  
The shell file can import the contents of `flake.nix` via [flake-compat](https://github.com/edolstra/flake-compat).

`flake.nix` provides an output `stack-shell.${system} = {version}: ...`.

- [output](https://github.com/deemp/flakes/blob/6b14dbcaaf9e2c310a6bddc5f2992bf328d3e868/templates/codium/haskell/flake.nix#L346)
- [definition](https://github.com/deemp/flakes/blob/6b14dbcaaf9e2c310a6bddc5f2992bf328d3e868/templates/codium/haskell/flake.nix#L239)

`stack.nix` imports `flake.nix` and provides a shell.

- [stack.nix](https://github.com/deemp/cachix/blob/17efcd60abe547d33bb2ccc63b561797a94e5b46/stack.nix)

## devShells

### Completions for CLI apps

```nix
devShells.default = pkgs.mkShell {
    buildInputs = [ packageExe ];
    shellHook = ''
      source ${packageExe}/share/bash-completion/completions/${packageExecutableName}
    '';
  };
```

## Troubleshooting

### Haskell Language Server

#### Clear cache

Sometimes, `HLS` finds errors in code despite `cabal build` running successfully.
If you use `VS Code`, find `Outputs` of `Haskell Language Server` and determine the cache directory for a project.

It should have approximately this form: `/home/<user name>/.cache/hie-bios/dist-<project-name>-<something>`.
Remove it and restart `HLS`.
