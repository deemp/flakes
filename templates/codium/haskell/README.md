# Haskell

`VSCodium` with extensions and executables for `Haskell`

`flake.nix` provides a devshell with tools and their descriptions

1. Run devshell

```console
nix develop
```

1. Write `settings.json`

1. Now, open `VSCodium` with extensions and executables for `Haskell`:

```console
codium .
```

1. Open a `Haskell` file `app/Main.hs` and hover over a function

1. Wait until `Haskell Language Server` starts giving you type info

## Stack + Nix integration

### Background

Suppose you'd like `Nix` to supply a C library [liblzma](https://tukaani.org/xz/) to `stack` using this integration.
You'd create a `stack-shell` (more on that below) in `flake.nix` and provide there a `Nix` package `pkgs.lzma`.
Then, `stack` will create an isolated environment, where this library is present, and run your program in this environment.
In such an environment, the program will have no access to installed programs like `rm` or `git`.
But what if your program needs to call these commands in your code?
In this case, their packages should also go into this isolated environment.
They will be turned into executables and become available to your program.
So, they should also be listed in `stack-shell`.

### This project

This sample `Haskell` project demonstrates `Stack` + `Nix` integration.

It has a Haskell `lzma` package as a dependency. This package depends on a `C` library `liblzma`.
`Nix` delivers this library as a package `pkgs.lzma` in `stack-shell`.

There's also a `pkgs.hello` package in `stack-shell`.
This allows `someFunc` from `src/Lib.hs` to call the `hello` command.

```console
stack run
```

This `hello` executable will also be available in `ghci`:

```console
stack ghci
ghci> :?
...
:!<command> run the shell command <command>
...
ghci> :! hello
Hello, world!

```

### Setup

Necessary components of `Stack` + `Nix` integration:

- `flake-compat` in `inputs` of `flake.nix`
  - This is to turn `stack-shell` in `flake.nix` into a valid [stack shell](https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file)
  - [repo](https://github.com/edolstra/flake-compat)
- Nix [enabled](https://docs.haskellstack.org/en/stable/nix_integration/#configuration-options) in `stack.yaml`
- `stack.nix`
  - The file should have the same name as the value of `shell-file` in `stack.yaml`
  - It will evaluate into a valid stack shell
- `stack-shell` with necessary derivations in `flake.nix`
  - The name `stack-shell` is chosen arbitrarily
  - The name should be the same as the one used in `stack.nix`

## manager

Can be useful if you'd like to write many small unrelated `Haskell` modules, maybe with a couple of other imported modules.

`manager` [flake](https://github.com/br4ch1st0chr0n3/flakes/tree/main/manager)

Get `manager` in a devshell:

```console
nix develop
manager
```

## Troubleshooting

See [Troubleshooting](https://github.com/br4ch1st0chr0n3/flakes#troubleshooting)
