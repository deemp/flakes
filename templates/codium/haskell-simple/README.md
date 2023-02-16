# Haskell

- `VSCodium` with extensions and executables for `Haskell`
- A sample `Haskell` project

Feel free to remove the `VSCodium`-related `Nix` code and whatever you want!

## Prerequisites

See these for additional info:

- [flake.nix](./flake.nix) - extensively commented code. Read it to understand how this flake works
- [language-tools/haskell](https://github.com/deemp/flakes/blob/main/language-tools/haskell/flake.nix) - this flake provides the `Haskell` tools in a convenient (IMHO) way
- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - an advanced version of this flake.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md) - general info about `Haskell` tools
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and run the app:

    ```console
    nix flake new my-project -t github:deemp/flakes#codium-haskell-simple
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

## Default devshell

Sometimes, `cabal` doesn't use the `Nix`-supplied packages ([issue](https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002)). In this case, use `cabal v1-*` - commands.

The `nix-managed` package that we provide the `devShells.default` for has several non-`Haskell` dependencies.

First, as `nix-managed` uses an `lzma` package, it needs a `C` library `liblzma`. This library is delivered via `Nix` as `pkgs.lzma`.

Second, `nix-managed` calls the `hello` command at runtime (see `someFunc` in `src/Lib.hs`). This command comes from a `hello` executable which is delivered via `Nix` as `pkgs.hello`.

1. We now enter a `devShell`.

    ```console
    nix develop
    ```

1. And run the app.

    ```console
    cabal run
    ```

1. Next, we can access the `hello` executable in a repl as this executable is on `PATH` of `cabal`.

    ```console
    cabal repl
    ghci> :?
    ...
    :!<command> run the shell command <command>
    ...
    ghci> :! hello
    Hello, world!
    ```

1. Furthermore, as `ghcid` uses a `cabal repl` command, when running `ghcid`, `hello` will still be available to the app.

    ```console
    ghcid
    ```

1. `ghcid` will run not only the `main` function, but also the code in magic comments (See `app/Main.hs`).

### Tools

## GHC

This template uses `GHC 9.2.5`. To switch to `GHC 9.0`,

1. In `flake.nix`, change GHC version from `"925"` to `"90"`.
1. If using `stack`, in `stack.yaml`, change `resolver` to [lts-19.33](https://www.stackage.org/lts-19.33) or a later one from `stackage`.

## Configs

- [package.yaml] - used by `stack` or `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.
