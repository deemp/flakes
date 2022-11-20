# manager

Manage repetitive `Haskell` modules and templates.

Sometimes, one wants to experiment with `Haskell` in multiple unrelated files. When creating such files, one's tasks can be:

- Make `Haskell Language Server` see all `Haskell` modules
- Invent new names for `stack`'s build targets
- Configure `ghcid` to run a specific function

`manager` simplifies these tasks.

Still, some actions like writing the correct dependencies and language extensions in `package.yaml` and `stack.yaml` should be done manually.

## Prerequisites

See [Prerequisites](https://github.com/br4ch1st0chr0n3/flakes#prerequisites)

## Usage

Goal: initialize a new project via `manager`. Start `VSCodium` with extensions and executables for `Haskell`.

1. Create a new project. `flake.nix` there will provide `VSCodium`, `manager`, and other tools in a devshell:

```console
nix shell github:br4ch1st0chr0n3/flakes/main?dir=manager
mkdir new-project
cd new-project
manager
# answer `y` if prompted
manager init
direnv allow
nix develop
write-settings-json
codium .
```

1. Open `README.md`

## Contribute

You can help improve `manager`

1. Clone the `flakes` [repo](https://github.com/br4ch1st0chr0n3/flakes)
1. Open `VSCodium` and start development:

  ```console
  cd manager
  nix develop nix-utils/
  codium .
  ```
