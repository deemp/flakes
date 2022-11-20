# Codium flake

Set up VSCodium with extensions and executables on its `PATH` in several lines of Nix code

See [Prerequisites](https://github.com/deemp/flakes#prerequisites)

## Contribute

```console
nix develop
codium .
```

## Troubleshooting

### GitHub Personal Access Token (PAT) for VS Codium extensions

- Create a `classic` PAT with permissions: `read:user, repo, user:email, workflow`
- Supply it to extensions

### Missing binaries on PATH in VSCodium

Case: VSCodium doesn't have the binaries provided in `runtimeDependencies` (like [here](https://github.com/deemp/flakes/blob/7bab5d96658007f5ad0c72ec7805b5b4eb5a83dd/templates/codium/generic/flake.nix#L33)) on `PATH`:

   1. You need to repair VSCodium's derivation (see [Repair a derivation](#))
   1. Assumptions:
      - current directory is `DIR`
      - there is a `DIR/flake.nix`
      - VSCodium is given as a derivation `codium`, like [here](https://github.com/deemp/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L25)
   1. In `DIR/flake.nix`, set `packages.default = codium;`, like [here](https://github.com/deemp/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L37)
   1. `Check`:
      1. `cd DIR`
      1. Start VSCodium: `nix run .#`
      1. Open a VSCodium terminal
      1. `echo $PATH` there
      1. It doesn't contain `/bin` dirs of specified `runtimeDependencies`
   1. Close:
      - devshells with this VSCodium
      - VSCodium itself
   1. Open a new terminal, `cd DIR`
   1. Run `nix store repair .#`
   1. Make a `Check` to verify binaries are on `PATH`
   1. If still no, continue
   1. Remove direnv profiles:
      - `cd DIR && rm -rf .direnv`
   1. Restart your OS
   1. `nix store gc` - collect garbage in Nix store - [man](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-store-gc.html)
   1. Again, make a `Check`
