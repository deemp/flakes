# flakes

Nix flakes for tools that I use

## Prerequisites

- Learn more about [flakes](https://github.com/br4ch1st0chr0n3/the-little-things#flakes)
- Learn how to [pin inputs](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html#flake-references)

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

## GitHub Personal Access Token for VS Codium

- permissions: `read:user, repo, user:email, workflow`

## Templates

### VSCodium

[VSCodium troubleshooting](#vscodium-troubleshooting)

### Generic

VSCodium with extensions and binaries

   ```console
   nix flake new codium-project -t github:br4ch1st0chr0n3/flakes#codium-generic
   git add codium-project
   cd codium-project
   nix run .# .
   ```

- Run `hello` in a VSCodium terminal

### Haskell

VSCodium with extensions and binaries for Haskell

   ```console
   nix flake new haskell-project -t github:br4ch1st0chr0n3/flakes#codium-haskell
   git add haskell-project
   cd haskell-project
   nix develop
   codium .
   ```

## Troubleshooting

### VSCodium troubleshooting

Case: VSCodium doesn't have the provided binaries on `PATH`:

   1. Check: VSCodium terminal -> `echo $PATH`
   1. You need to repair its [derivation](https://nixos.org/manual/nix/unstable/language/derivations.html?highlight=derivation#derivations)
   1. It's assumed that in your flake, `packages.default = codium;`
   1. Get `CODIUM_PATH` - path of VSCodium in Nix store:

      ```console
      nix show-derivation .# | jq -r ".[].outputs.out.path"
      ```

   1. Copy this path
   1. Close windows and rm files that may access this codium's executable:
      - terminal with this project's devshell
      - VSCodium
   1. Now, check that this path is not alive in a new terminal. This command should show no files and processes:

      ```console
      nix-store --query --roots CODIUM_PATH
      ```

   1. Remove the listed files and terminate processes
   1. Repair this executable:

      ```console
      nix store repair CODIUM_PATH
      ```

   1. Finally, try rerunning codium
