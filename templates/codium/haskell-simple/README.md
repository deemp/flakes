# Haskell

- `VSCodium` with extensions and executables for `Haskell`.
- A sample `Haskell` project.

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
- [hie.yaml] - a config for [hie-bios](https://github.com/haskell/hie-bios). Can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) to check the `Haskell Language Server` setup.
