# manager

Manage repetitive Haskell modules and templates

Sometimes, one wants to experiment with Haskell in multiple unrelated files. When creating such files, one's tasks can be:

- Make Haskell Language Server see all Haskell modules
- Invent new names for stack's build targets
- 

## Use in a new project

```console
nix shell github:br4ch1st0chr0n3/flakes/main?dir=manager
# see available commands
manager
manager init
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

## Sample projects

- [nix-managed](https://github.com/br4ch1st0chr0n3/nix-managed)

## Dependencies

- [hpack](https://github.com/sol/hpack)
- [implicit-hie](https://github.com/Avi-D-coder/implicit-hie)
