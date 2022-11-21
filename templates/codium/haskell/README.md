# Haskell

`VSCodium` with extensions and executables for `Haskell`

## Prerequisites

- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)

## Quick start

1. Run a devshell

```console
nix develop
write-settings-json
codium .
```

1. Open a `Haskell` file `app/Main.hs` and hover over a function

1. Wait until `Haskell Language Server` starts giving you type info

## Stack + Nix integration

### Background

Suppose you'd like `Nix` to supply a C library [liblzma](https://tukaani.org/xz/) to `stack` using this integration.
You'd create a `stack-shell` (more on that below) in `flake.nix` and provide there a `Nix` package `pkgs.lzma`.
Then, `stack` will create an isolated environment, where this library is present, and run your program in this environment.
In such an environment, your program won't have an access to other libraries and programs like `rm` or `git`.
But what if your program needs to call the `rm` command?
In this case, your `stack-shell` should contain the relevant package, `pkgs.coreutils`.
This package will be turned into executables. Then, `rm` and some other commands will become available in that isolated environment.

### This project

This sample `Haskell` project demonstrates `Stack` + `Nix` integration.

It has a Haskell `lzma` package as a dependency (see [package.yaml](./package.yaml)). This package depends on a `C` library `liblzma`.
`Nix` delivers this library as a package `pkgs.lzma` in `stack-shell`.

There's also a `pkgs.hello` package in `stack-shell`.
This allows `someFunc` from `src/Lib.hs` to call the `hello` as a shell command.

```console
stack run
```

This `hello` executable will also be available in `ghci` as a shell command:

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
  - This is to turn `stack-shell` in `flake.nix` into a valid [stack shell](https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file) in `stack.nix`
  - [repo](https://github.com/edolstra/flake-compat)
- Nix [enabled](https://docs.haskellstack.org/en/stable/nix_integration/#configuration-options) in `stack.yaml`
- `stack.nix`
  - The file should have the same name as the value of `shell-file` in `stack.yaml`
- `stack-shell` with necessary derivations in `flake.nix`
  - The name `stack-shell` is chosen arbitrarily
  - The name should be the same as the one used in `stack.nix`

## Tools

### ghcid

`ghcid` is a `Very low feature GHCi based IDE`s. It can be used to rerun a function in a given file on changes in a given directory.

### GHC

Switch to `GHC 9.0.2`:

- in `flake.nix`, change `"92"` to `"90"`
- in `stack.nix`, change `"924"` to `"902"`
- in `stack.yaml`, change `resolver` to [lts-19.33](https://www.stackage.org/lts-19.33) or a later one from `stackage`

### managers

`manager` can be useful if you'd like to write many small unrelated `Haskell` modules, maybe with a couple of other imported modules.

See [manager](https://github.com/deemp/flakes/tree/main/manager)

Get `manager` in a devshell:

```console
nix develop
manager
```
