# Haskell

VSCodium with extensions and executables for Haskell

## Nix + Stack

This sample Haskell project demonstrates `Stack` + `Nix` integration.

It has an `lzma` dependency that itself has a `C` language dependency.
This `C` dependency is delivered as a derivation via Nix - see `pkgs.lzma` in `flake.nix`.

There's also a `pkgs.hello` derivation in `stack-shell`.
This allows `someFunc` from `src/Lib.hs` to call the `hello` executable.

```console
stack run
```

This `hello` executable will also be available in `ghci`:

```console
stack ghci
ghci> :! hello
```

Necessary components of `Stack` + `Nix` integration:

- `flake-compat` in `inputs` of `flake.nix`
- Nix [enabled](https://docs.haskellstack.org/en/stable/nix_integration/#configuration-options) in `stack.yaml`
- `stack.nix` - an appropriate [stack shell](https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file)
  - the file should have the same name as the value of `shell-file` in `stack.yaml`
- `stack-shell` with necessary derivations in `flake.nix`
  - the name `stack-shell` is chosen arbitrarily
  - the name should be the same as the one used in `stack.nix`

## manager

Can be useful if you'd like to write many small unrelated `Haskell` modules, maybe with a couple of other imported modules.

### Get manager

When in this repo:

```console
nix develop
manager
```

From a remote repo:

```console
nix shell github:br4ch1st0chr0n3/flakes/main?dir=manager
manager
```
