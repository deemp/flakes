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
- [manager](./manager/README.md)
  - Automate routine actions in projects having multiple unrelated `Haskell` modules
- [source-flake](./source-flake/) - pinned flakes
  - I use them to sync flake inputs in my other flakes
- [templates](./templates/) - Nix flake templates
  - See [Templates](#templates)

## Other flakes

- [hpack](https://github.com/deemp/hpack) - `0.35.0`, until it's on `nixpkgs`
- [refmt](https://github.com/deemp/refmt) - `HCL` <-> `JSON` <-> `YAML` converter
- [terrafix](https://github.com/deemp/terrafix) - generate `Terraform` files from DRY `Nix` expressions
- [try-phi](https://github.com/objectionary/try-phi) - online interactive translator and interpreter of EO and 𝜑-calculus
- [devops-labs](https://github.com/deemp/devops-labs) - `Nix` in devops projects
- [blockchain](https://github.com/deemp/blockchain) - A repo for `BDLD` course
- [scala](https://github.com/deemp/scala) - task solutions and notes on `Scala` courses
- [lens-examples](https://github.com/deemp/lens-examples) - examples of `Haskell` lens usage

## Templates

This repo provides several templates. Learn how you can use them.

- Templates [Prerequisites](./README/NixPrerequisites.md#templates)
- Dev tools [Conventions](./README/Conventions.md#dev-tools) - decide where to put flakes generated from templates
