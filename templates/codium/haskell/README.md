# Haskell

VSCodium with extensions and binaries for Haskell

## Sample project

This sample Haskell project demonstrates how to build a Haskell library `lzma` that uses a `C` dependency
This dependency is delivered via Nix - see `pkgs.lzma` in `flake.nix`.

## Nix + stack

Nix can provide libraries to `stack`. Necessary components:

- `flake-compat` in `inputs` of `flake.nix`
- Nix [enabled](https://docs.haskellstack.org/en/stable/nix_integration/#configuration-options) in `stack.yaml`
- `stack.nix` - an appropriate [stack shell](https://docs.haskellstack.org/en/stable/nix_integration/#external-c-libraries-through-a-shellnix-file)
  - the file should have the same name as the value of `shell-file` in `stack.yaml`
- `stack-shell` with necessary libraries in `flake.nix`
  - the name `stack-shell` is chosen arbitrarily
  - the name should be the same as the one used in `stack.nix`
