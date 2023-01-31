# flakes

Nix flakes for tools that I use

## Prerequisites

- [Nix prerequisites](./README/NixPrerequisites.md)
- [Conventions](./README/Conventions.md)
- [Troubleshooting](./README/Troubleshooting.md)

## Contents

- [codium](./codium/README.md) - set up `VSCodium` with extensions and executables on `PATH`
- [devshell](./devshell/README.md) - Easily create a CLI to your `devShells`
- [drv-tools](./drv-tools/flake.nix) - convenience tools for working with derivations
- [env2json](./env2json/README.md) - convert `.env` to `.json`
- [flakes-tools](./flakes-tools/flake.nix) - convenience tools for working with flakes and pushing them to [cachix](https://www.cachix.org/)
- [json2md](./json2md/README.md) - convert `JSON` (or `Nix`) to `Markdown`
  - I use it to generate `.md` docs from Nix expressions - [example](https://github.com/deemp/devops-labs/blob/0ae9881ab58b99f114aaf21cb5cad85f2ce37e40/.nix/write-configs.nix#L26)
- [language-tools](./flakes-tools) - tools for languages that I use
  - `Haskell`, `Python`, `PureScript`, `nix`
- [lima](./lima) - convert between `Literate Haskell` (`.lhs`) or `Haskell` (`.hs`) and `Markdown` (`.md`) with [round-trip property](https://hedgehog.qa/article/haskell-round-trip) usually kept
- [source-flake](./source-flake/) - pinned flakes
  - I use them to sync flake inputs in my flakes
- [templates](./templates/) - Nix flake templates
  - See [Templates](#templates)
- [terrafix](./terrafix) - `eDSL` to generate `Terraform` files from DRY `Nix` expressions
- [workflows](./workflows) - generate `GH Actions` Workflows from DRY `Nix` expressions

## Projects

Repository with my projects:

- [projects](https://github.com/deemp/projects)

Other projects in separate repositories:

- [devops-labs](https://github.com/deemp/devops-labs) - `Nix` in devops projects
- [try-phi](https://github.com/objectionary/try-phi) - online interactive translator and interpreter of EO and 𝜑-calculus

Nix-packaged:

- [refmt](https://github.com/deemp/refmt) - `HCL` <-> `JSON` <-> `YAML` converter

## Templates

### Prerequsites

This repo provides several templates. Learn how you can use them:

- Templates [Prerequisites](./README/NixPrerequisites.md#templates)
- Dev tools [Conventions](./README/Conventions.md#dev-tools) - decide where to put flakes generated from templates

### Available templates

- [codium-generic](./templates/codium/generic/README.md) - `VSCodium` with extensions and executables on `PATH`.
- [codium-haskell](./templates/codium/codium-haskell/README.md) - `VSCodium` with extensions and executables on `PATH` for `Haskell` (see ). Demonstrates several ways to run a `Haskell` app.
- see [codium-haskell-simple](./templates/codium/codium-haskell/README.md) - `VSCodium` with extensions and executables on `PATH` for `Haskell`. A simplified version of `codium-haskell`.
