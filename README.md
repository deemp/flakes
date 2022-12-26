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
- [json2md](./json2md/README.md) - convert `JSON` to `Markdown`
  - Can be combined with `builtins.toJSON` and used to generate `.md` docs from Nix expressions - [example](https://github.com/deemp/devops-labs/blob/0ae9881ab58b99f114aaf21cb5cad85f2ce37e40/.nix/write-configs.nix#L26)
- [language-tools](./flakes-tools) - tools for languages that I use
- [lima](./lima) - convert between `Literate Haskell` (`.lhs`) and `Markdown` (`.md`) with [round-trip property](https://hedgehog.qa/article/haskell-round-trip) usually kept (a fork of [LiterateMarkdown](https://github.com/haskie-lambda/LiterateMarkdown))
- [manager](./manager) - a `Haskell` CLI app for managing projects with multiple `Main.hs` modules
- [terrafix](./terrafix) - generate `Terraform` files from DRY `Nix` expressions
- [source-flake](./source-flake/) - pinned flakes
  - I use them to sync flake inputs in my flakes
- [templates](./templates/) - Nix flake templates
  - See [Templates](#templates)

## Projects

Repository with my projects:

- [projects](https://github.com/deemp/projects)

Other projects in separate repositories:

- [devops-labs](https://github.com/deemp/devops-labs) - `Nix` in devops projects
- [try-phi](https://github.com/objectionary/try-phi) - online interactive translator and interpreter of EO and 𝜑-calculus

Nix-packaged:

- [hpack](https://github.com/deemp/hpack) - `0.35.0`, until it's fixed on `nixpkgs`
- [refmt](https://github.com/deemp/refmt) - `HCL` <-> `JSON` <-> `YAML` converter

## Templates

This repo provides several templates. Learn how you can use them.

- Templates [Prerequisites](./README/NixPrerequisites.md#templates)
- Dev tools [Conventions](./README/Conventions.md#dev-tools) - decide where to put flakes generated from templates
