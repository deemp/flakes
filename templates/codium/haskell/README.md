# Haskell

- `VSCodium` with extensions and executables for `Haskell`
- A sample `Haskell` project
- Several ways to run a `Haskell` app

Feel free to remove the `VSCodium`-related `Nix` code and whatever you want!

## Prerequisites

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium`
- [codium-haskell-simple](https://github.com/deemp/flakes/tree/main/templates/codium/haskell-simple#readme) - a simplified version of this flake
- [flake.nix](./flake.nix) - extensively commented code
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and run the app:

    ```console
    nix flake new my-project -t github:deemp/flakes#codium-haskell
    cd my-project
    git init && git add
    nix develop
    cabal run
    ```

1. Write `settings.json` and start `VSCodium`:

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. Open a `Haskell` file `app/Main.hs` and hover over a function.

1. Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

## Run a Haskell app

Below is the comparison of ways to run a `Haskell` app. I prefer to use everything apart from `stack` and `shellFor`.

### Cabal

**Advantages** - medium setup, can build incrementally

**Disadvantages** - not that reproducible builds

Incremental builds via `cabal` + `Nix`-provided packages. A devshell will run the app via `cabal run`.

```console
nix develop .#cabal
```

If you use `hpack` to generate `cabal`, see [here](https://github.com/sol/hpack) what can go into a `package.yaml`.

### Nix

**Advantages** - medium setup, reproducible build, make a standalone executable from a package.

**Disadvantages** - no incremental builds (rebuilds the whole project from scratch on slight code changes)

`Nix` provides necessary packages, binaries and libraries to the app. A devshell will run the app

```console
nix develop .#nixPackaged
```

### Docker

**Advantages** - medium setup, reproducible lightweight container from a package

**Disadvantage** - no incremental builds

Put an executable into a Docker image and run it:

```console
nix develop .#docker
```

### Cabal + Nix integration

**Advantages** - easy setup, incremental builds

**Disadvantage** - need to start a shell

Make a shell with all deps available and build incrementalllt via `cabal`

```console
nix develop .#cabalShellFor
```

### Stack + Nix integration

**Advantage** - uses stack for incremental builds, very easy setup

**Disadvantage** - doesn't use Nix caches and takes packages from Stackage

#### Background

Suppose you'd like `Nix` to supply a `C` library [liblzma](https://tukaani.org/xz/) to `stack` using [this integration](https://docs.haskellstack.org/en/stable/nix_integration/).
You'd create a `stack-shell` (more on that below) in `flake.nix` and provide there a `Nix` package `pkgs.lzma`.
Then, `stack` will create an isolated environment, where this library is present, and run your program in this environment.
In such an environment, your program won't have an access to other libraries and programs like `rm` or `git`.
But what if your program needs to call the `rm` command?
In this case, your `stack-shell` should contain the relevant package, `pkgs.coreutils`.
This package will be turned into executables. Then, `rm` and some other commands will become available in that isolated environment.

#### Stack guide

This sample `Haskell` project demonstrates `Stack` + `Nix` integration.

It has a Haskell `lzma` package as a dependency (see [package.yaml](./package.yaml)). This package depends on a `C` library `liblzma`.
`Nix` delivers this library as a package `pkgs.lzma` in `stack-shell`.

There's also a `pkgs.hello` package in `stack-shell`.
This allows `someFunc` from `src/Lib.hs` to call the `hello` as a shell command.

```console
nix develop .#stack
```

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

Furthermore, as `ghcid` uses a `stack ghci` command, you can run `ghcid` as follows:

```console
ghcid
```

Additionally, `ghcid` will run the code in magic comments (See `app/Main.hs`).

#### Setup

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

## GHC

This template uses `GHC 9.2`. You can switch to `GHC 9.0`:

- In `flake.nix`, change `"92"` to `"90"`
- If using `stack`, in `stack.yaml`, change `resolver` to [lts-19.33](https://www.stackage.org/lts-19.33) or a later one from `stackage`

## Configs

- [package.yaml] - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.
