# manager

Manage repetitive `Haskell` modules and templates.

Sometimes, one wants to experiment with `Haskell` in multiple unrelated files. When creating such files, one's tasks can be:

- Make `Haskell Language Server` see all `Haskell` modules
- Invent new names for `stack`'s build targets
- Configure `ghcid` to run a specific function

`manager` simplifies them.

Still, some actions like writing the correct dependencies and language extensions in `package.yaml` and `stack.yaml` should be done manually.

## Usage

Goal: initialize a new project via `manager`. Start `VSCodium` with extensions and executables for `Haskell`.

1. Install `Nix` - complete all steps from [here](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/InstallNix.md)

1. Create a new project. `flake.nix` there will provide `VSCodium`, `manager`, and other tools in a devshell:

```console
nix shell github:br4ch1st0chr0n3/flakes/main?dir=manager
mkdir new-project
cd new-project
manager
manager init
direnv allow
nix develop
write-settings-json
codium .
```

1. Open `README.md`

1. In case of problems see [Troubleshooting](https://github.com/br4ch1st0chr0n3/flakes#troubleshooting)

## Develop this project

Open IDE:

  ```sh
  nix develop nix-utils
  codium .
  ```

Build

  ```sh
  stack build
  ```
