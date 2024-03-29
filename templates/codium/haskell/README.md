# Haskell

- `VSCodium` with extensions and executables for `Haskell`.
- A sample `Haskell` project.
- Several ways to run a `Haskell` app.

## Prerequisites

<details>

  <summary>Spoiler</summary>

- NixOS wiki - [Haskell](https://nixos.wiki/wiki/Haskell)
- [flake.nix](./flake.nix) - code in this flake is extensively commented.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - template for this flake.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md) - general info about `Haskell` tools.
- [language-tools/haskell](https://github.com/deemp/flakes/blob/main/language-tools/haskell/flake.nix) - a flake that provides `Haskell` tools.
- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` with extensions.
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [flakes](https://github.com/deemp/flakes#readme) - my Nix flakes that may be useful for you.
- [Conventions](https://github.com/deemp/flakes/blob/main/README/Conventions.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

</details>

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and run the app.

    ```console
    nix flake new my-project -t github:deemp/flakes#codium-haskell
    cd my-project
    git init && git add
    nix develop
    cabal run
    ```

1. Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. Open a `Haskell` file `app/Main.hs` and hover over a function.

1. Wait until `Haskell Language Server` (`HLS`) starts giving you type info.

## Default devshell

The `devShells.default` here is similar to the [cabal](#cabal) shell.

The `nix-managed` package (package in this flake) has several non-`Haskell` dependencies.

First, as `nix-managed` uses an `lzma` package, it needs a `C` library `liblzma`. This library is delivered via `Nix` as `pkgs.lzma`.

Second, `nix-managed` calls the `hello` command at runtime (see `someFunc` in `src/Lib.hs`). This command comes from a `hello` executable which is delivered via `Nix` as `pkgs.hello`.

`cabal` from `devShells.default` has on its `PATH` that `hello` executable.

Let's inspect what's available.

1. Enter the `devShell`.

    ```console
    nix develop
    ```

1. Run the app.

    ```console
    cabal run
    ```

1. Next, access the `hello` executable in a repl.

    ```console
    cabal repl
    ghci> :?
    ...
    :!<command> run the shell command <command>
    ...
    ghci> :! hello
    Hello, world!
    ```

1. `ghcid` uses the `cabal repl` command. That's why, when running `ghcid`, `hello` will be available to the app.

    ```console
    ghcid
    ```

1. `ghcid` will run not only the `main` function, but also the code in magic comments (See `app/Main.hs`).

1. Sometimes, `cabal` doesn't use the `Nix`-supplied packages ([issue](https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002)). In this case, use `cabal v1-*` - commands.

## Run a Haskell app

Below is the comparison of ways to run a `Haskell` app.
I prefer to use anything but `stack`.

### shellFor

**Advantages** - incremental builds, can pass the build dependencies to [devshell](https://github.com/numtide/devshell) (See the [PR](https://github.com/numtide/devshell/pull/261))

Make a shell with all deps available and build incrementally via `cabal`.

```console
nix develop .#shellFor
```

### Cabal

**Advantages** - can build incrementally

**Disadvantages** - not that reproducible builds

Incremental builds via `cabal` + `Nix`-provided packages. A devshell will run the app via `cabal run`.

```console
nix develop .#cabal
```

### Binary

**Advantages** - reproducible build, make an executable binary app from a Haskell executable.

**Disadvantages** - no incremental builds (rebuilds the whole project from scratch on slight code changes)

`Nix` provides necessary packages, binaries and libraries to the app. A devshell will run the app

```console
nix develop .#binary
```

### Docker

**Advantages** - reproducible lightweight container from a package

**Disadvantage** - no incremental builds

Put an executable into a Docker image and run it:

```console
nix develop .#docker
```

### Stack

**Advantage** - uses stack for incremental builds, very easy setup

**Disadvantage** - doesn't use Nix caches and takes packages from Stackage

```console
nix develop .#stack
```

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

### Available versions

This flake uses GHC of a specific version (`ghcVersion`).

`nixpkgs` provides other `GHC` versions.
Explore them in a repl:

```console
nix repl
:lf .
-- use your system
ghcVersions.x86_64-linux
```

### Change version

To switch to a specific `GHC` version (let's call it `<ghc>`):

1. In `flake.nix`, change the `ghcVersion` value to `<ghc>`.
1. If you're using `stack`, in `stack.yaml`, change the `resolver` to match the `<ghc>` version.

## Configs

- [package.yaml](./package.yaml) - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- [hie.yaml] - a config for [hie-bios](https://github.com/haskell/hie-bios). Can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) to check the `Haskell Language Server` setup.
