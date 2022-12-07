# Haskell

`VSCodium` with extensions and executables for `Haskell`

## Prerequisites

- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, run `VSCodium` from a devshell:

```console
nix flake new my-project -t github:deemp/flakes#codium-haskell
cd my-project
git init && git add . && git commit -m "init"
nix develop
write-settings-json
codium .
```

1. Open a `Haskell` file `app/Main.hs` and hover over a function.

1. Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

## Stack + Nix integration

### Background

Suppose you'd like `Nix` to supply a C library [liblzma](https://tukaani.org/xz/) to `stack` using [this integration](https://docs.haskellstack.org/en/stable/nix_integration/).
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

Furthermore, as `ghcid` (see [ghcid](#ghcid)) uses a `stack ghci` command, you can run `ghcid` as follows:

```console
ghcid
```

Additionally, `ghcid` will run the code in magic comments (See `app/Main.hs`).

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

[ghcid](https://github.com/ndmitchell/ghcid) is a `Very low feature GHCi based IDE`.
It can be used to rerun a function in a given file on changes in a given directory.
This template provides a sample configuration for this tool in the `.ghcid` file.

### GHC

This template uses `GHC 9.2.4`. You can switch to `GHC 9.0.2`:

- in `flake.nix`, change `"92"` to `"90"`
- in `stack.yaml`, change `resolver` to [lts-19.33](https://www.stackage.org/lts-19.33) or a later one from `stackage`

After that, if you're working in `VSCodium`, you should repeat the [Quick Start](#quick-start)
