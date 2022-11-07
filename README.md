# flakes

Nix flakes for tools that I use

`nix develop .#enter` to enter a `fish` shell

- Provide the cachix cache name in a `.env` file in format `CACHIX_CACHE=some-cache-name`

## Pushing conventions

1. Before a commit:
   - Use `flakes-toggle-in-each-dir` to switch to `path:github` if you used `path:./...`

1. Commit and push

1. Wait a couple of minutes for a GH action to complete updating `flake.lock`-s and for `git` to fetch the latest changes
   - Check the action's progress in the `GitHub Actions` extension

1. `git rebase` to get your changes from GH

## Substituters and keys

There are `extra-trusted-public-keys`, `extra-trusted-public-keys` in [flake.nix](./flake.nix). If a substituter like `cachix` fails, comment out the lines containing its address

## Templates

### Codium

[codium](./codium/template/flake.nix) includes extensions and binaries

```sh
nix flake new codium-project -t github:br4ch1st0chr0n3/flakes#codium
cd codium-project
nix run
```

- may need to reload the computer for extensions to become available
