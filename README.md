# flakes

Nix flakes for tools that I use

## Prerequisites

- [Nix prerequisites](./README/Prerequisites.md#nix)
- [Conventions](./README/Conventions.md)
- [Troubleshooting](./README/Troubleshooting.md)

## Contents

- [codium](./codium/README.md) - set up VSCodium with binaries on `PATH` and extensions
- [devshell](./devshell/README.md) - Easily create a CLI to your `devShells`
- [drv-tools](./drv-tools/flake.nix) - convenience tools for working with derivations
- [env2json](./env2json/README.md) - convert `.env` to `.json`
- [flakes-tools](./flakes-tools/flake.nix) - convenience tools for working with flakes and pushing them to [cachix](https://www.cachix.org/)
- [json2md](./json2md/README.md) - convert `JSON` to `Markdown`
  - Can be combined with `builtins.toJSON` and used to generate `.md` docs from Nix expressions - [example](https://github.com/br4ch1st0chr0n3/devops-labs/blob/0ae9881ab58b99f114aaf21cb5cad85f2ce37e40/.nix/write-configs.nix#L26)
- [language-tools](./flakes-tools) - tools for languages that I use
- [manager](./manager/README.md)
  - automate routine actions in projects having multiple unrelated `Haskell` modules
- [source-flake](./source-flake/) - pinned flakes
  - used to have the same flake inputs in my flakes
- [templates](./templates/) - Nix flake templates that can be used to initialize new projects
  - see the [Templates](#templates) section

## Other flakes

- [hpack](https://github.com/br4ch1st0chr0n3/hpack) - `0.35.0`, until it's on `nixpkgs`
- [refmt](https://github.com/br4ch1st0chr0n3/refmt) - `HCL` <-> `JSON` <-> `YAML` converter
- [terrafix](https://github.com/br4ch1st0chr0n3/terrafix) - generate `Terraform` files from DRY `Nix` expressions
- [try-phi](https://github.com/objectionary/try-phi) - online interactive translator and interpreter of EO and 𝜑-calculus
- [devops-labs](https://github.com/br4ch1st0chr0n3/devops-labs) - `Nix` in devops projects
- [blockchain](https://github.com/br4ch1st0chr0n3/blockchain) - A repo for `BDLD` course

## Templates

This repo provides several templates. Learn how you can use them.

- Templates [prerequisites](./README/Prerequisites#templates)
- Dev tools [conventions](./README/Conventions.md#dev-tools) - to decide where to put the templates

## TODO

<!-- TODO -->
- Update flakes' inputs when this [issue](https://github.com/NixOS/nix/issues/5790#issuecomment-1315831247) is fixed
- drop `hpack` fork when this `hpack` [issue](https://github.com/sol/hpack/issues/528) is resolved
