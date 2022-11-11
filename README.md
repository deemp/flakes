# flakes

Nix flakes for tools that I use

## Templates

### VSCodium

[template](./codium/template/flake.nix) - VSCodium with extensions and binaries

- May need to reload the computer for extensions to become available

   ```sh
   nix flake new codium-project -t github:br4ch1st0chr0n3/flakes#codium
   git add codium-project
   cd codium-project
   nix run .# .
   ```

- Run `hello` in a VSCodium terminal

## Pushing conventions

1. Commit and push

1. Wait a couple of minutes for a GH action to complete updating `flake.lock`-s and for `git` to fetch the latest changes
   - Check the action's progress in the `GitHub Actions` extension

1. `git rebase` to get your changes from GH

## Substituters and keys

There are `extra-trusted-public-keys`, `extra-trusted-public-keys` in [flake.nix](./flake.nix). If a substituter like `cachix` fails, comment out the lines containing its address

## devhshell

Easily create a CLI to your devShells commands

- devshell [repo](https://github.com/numtide/devshell)
- devshell [tutorial](https://yuanwang.ca/posts/getting-started-with-flakes.html#numtidedevshell)
