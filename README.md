# flakes

Nix flakes for tools that I use

`nix develop .#enter` to enter a `fish` shell

- Provide the cachix cache name in a `.env` file in format `CACHIX_CACHE=some-cache-name`

## Troubleshoot pushing

- Double pushing - `push-to-github`
  1. Switch to `path:github` and update flakes with whatever existing content
  2. Push flakes. Their contents becone available on GH
  3. Update flakes again, taking their inputs from GH
  4. Push them
  5. Restore relative paths in flakes' inputs: `path:./.`
