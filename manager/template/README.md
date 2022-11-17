# Haskell project with `manager`

`flake.nix` provides a devshell with tools and their descriptions

1. Run devshell

```console
nix develop
```

1. Write `settings.json`

1. Learn about `manager`:

```console
manager
```

1. Now, open `VSCodium` with extensions and executables for `Haskell`:

```console
codium .
```

1. Open a `Haskell` file `Modules/B/Main.hs` and hover over a function

1. Wait until `Haskell Language Server` starts giving you type info

1. In case of problems see [Troubleshooting](https://github.com/br4ch1st0chr0n3/flakes#troubleshooting)

## Used flakes

`VSCodium` flake - [here](https://github.com/br4ch1st0chr0n3/flakes/tree/main/templates/codium/haskell)
`manager` flake - [here](https://github.com/br4ch1st0chr0n3/flakes/tree/main/manager)

## Nix + Stack

This sample `Haskell` project demonstrates `Stack` + `Nix` integration.

It has an `lzma` dependency that itself has a `C` language dependency.
This `C` dependency is delivered as a derivation via `Nix` - see `pkgs.lzma` in `flake.nix`.

There's also a `pkgs.hello` derivation in `stack-shell`.
This allows `someFunction` from `Modules/B/AnotherModule.hs` to call the `hello` executable.

```console
stack run B
```

This `hello` executable will also be available in `ghci`:

```console
stack ghci Modules/B/Main.hs
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
