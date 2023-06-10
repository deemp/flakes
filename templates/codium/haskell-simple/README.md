# Haskell

- `VSCodium` with extensions and executables for `Haskell`.
- A sample `Haskell` project.

Feel free to remove the `VSCodium`-related `Nix` code and whatever you want!

## Prerequisites

- NixOS wiki - [Haskell](https://nixos.wiki/wiki/Haskell)

<details>

  <summary>Spoiler</summary>

- [flake.nix](./flake.nix) - code in this flake is extensively commented.
- [language-tools/haskell](https://github.com/deemp/flakes/blob/main/language-tools/haskell/flake.nix) - a flake that conveniently provides `Haskell` tools.
- [Conventions](https://github.com/deemp/flakes/blob/main/README/Conventions.md#dev-tools) - I recommended to use this flake just for development. For packaging an app, make another flake with a limited number of inputs to reduce the `flake.lock` size.

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` with extensions.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - an advanced version of this flake.
  - Shows how to build a static binary from your package and how to make a Docker image with it.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md) - general info about `Haskell` tools.
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [Nixpkgs support for incremental Haskell builds](https://www.haskellforall.com/2022/12/nixpkgs-support-for-incremental-haskell.html)
- [flakes](https://github.com/deemp/flakes#readme) - my Nix flakes that may be useful for you.

</details>

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and run the app.

    ```console
    nix flake new my-project -t github:deemp/flakes#codium-haskell-simple
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

## Configs

- [package.yaml](./package.yaml) - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.
