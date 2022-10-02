# flakes

Nix flakes for tools that I use

`nix develop .#enter` to enter a `fish` shell

- Provide the cachix cache name in a `.env` file in format `CACHIX_CACHE=some-cache-name`

## Pushing conventions

1. before a commit:
   - Use `flakes-toggle-in-each-dir` to switch to `path:github` if you used `path:./...`

1. Commit and push

1. Wait a couple of minutes for a GH action to complete updating `flake.lock`-s and for git fetcher to fetch the latest changes. You may check the action's progress in the `GitHub Actions` extension

1. `git rebase` to get your changes from GH
