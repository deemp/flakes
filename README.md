# flakes

Nix flakes for tools that I use.

## Prerequisites

- [Nix prerequisites](./README/NixPrerequisites.md)
- [Conventions](./README/Conventions.md)
- [Troubleshooting](./README/Troubleshooting.md)

## Flakes

- [codium](./codium/README.md) - set up `VSCodium` with extensions and executables on `PATH`
- [devshell](./devshell/README.md) - Easily create a CLI to your `devShells`
- [drv-tools](./drv-tools/flake.nix) - convenience tools for working with derivations
- [env2json](./env2json/README.md) - convert `.env` to `.json`
- [flakes-tools](./flakes-tools/flake.nix) - convenience tools for working with flakes and pushing them to [cachix](https://www.cachix.org/)
- [json2md](./json2md/README.md) - convert `JSON` (or `Nix`) to `Markdown`
  - I use it to generate `.md` docs from Nix expressions - [example](https://github.com/deemp/devops-labs/blob/0ae9881ab58b99f114aaf21cb5cad85f2ce37e40/.nix/write-configs.nix#L26)
- [language-tools](./flakes-tools) - tools for languages that I use
  - `Haskell`, `Python`, `PureScript`, `nix`
- [source-flake](./source-flake/) - pinned flakes
  - I use them to sync flake inputs in my flakes
- [templates](./templates/) - Nix flake templates
  - See [Templates](#templates)
- [terrafix](./terrafix) - `eDSL` to generate `Terraform` files from DRY `Nix` expressions
- [workflows](./workflows) - generate `GH Actions` Workflows from DRY `Nix` expressions

## Templates

### Prerequsites

This repo provides several templates. Learn how you can use them:

- Templates [Prerequisites](./README/NixPrerequisites.md#templates)
- Dev tools [Conventions](./README/Conventions.md#dev-tools) - decide where to put flakes generated from templates

### Available templates

- [codium-generic](./templates/codium/generic/README.md) - `VSCodium` with extensions and executables on `PATH`.
- [codium-haskell](./templates/codium/haskell/README.md) - `VSCodium` with extensions and executables on `PATH` for `Haskell`. Demonstrates several ways to run a `Haskell` app.
- [codium-haskell-simple](./templates/codium/haskell-simple/README.md) - `VSCodium` with extensions and executables on `PATH` for `Haskell`. A simplified version of `codium-haskell`.
- [haskell-minimal](./templates/haskell-minimal/README.md) - A minimal flake for developing local `Haskell` packages.
