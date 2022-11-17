# manager

Manage repetitive Haskell modules and templates

Sometimes, one wants to experiment with Haskell in multiple unrelated files. When creating such files, one's tasks can be:

- Make Haskell Language Server see all Haskell modules
- Invent new names for `stack`'s build targets
- Configure `ghcid` to run a specific function

`manager` simplifies them.

Still, some actions like writing the correct dependencies and language extensions in `package.yaml` and `stack.yaml` should be done manually.

## Usage

Create a new project. `flake.nix` there will provide `manager` in a devshell

```console
mkdir new-project
cd new-project
nix shell github:br4ch1st0chr0n3/flakes/main?dir=manager
manager
manager init
nix develop
```

## Run project

Open IDE:

  ```sh
  nix develop nix-utils
  ```

Build project

  ```sh
  stack build
  ```
