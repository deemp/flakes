# flakes

Nix flakes for tools that I use

## Prerequisites

- Learn about [flakes](https://github.com/br4ch1st0chr0n3/the-little-things#flakes)
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
   cd codium-project
   git init
   git add .
   nix run .# .
   ```

- Run `hello` in a VSCodium terminal

### Haskell

VSCodium with extensions and binaries for Haskell

   ```console
   nix flake new haskell-project -t github:br4ch1st0chr0n3/flakes#codium-haskell
   cd haskell-project
   git init
   git add .
   nix develop
   codium .
   ```

## Troubleshooting

## Repair a derivation

[Derivation](https://nixos.org/manual/nix/unstable/language/derivations.html?highlight=derivation#derivations)

See [manual](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-store-repair.html)

Set a  `packages.default = your-corrupt-derivation` in `flake.nix` and then run `nix store repair .#`
   - Learn more about [installables](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix.html?highlight=installable#installables)

### VSCodium troubleshooting

Case: VSCodium doesn't have the provided binaries on `PATH`:

   1. `Check`: Open a VSCodium terminal -> `echo $PATH`
   1. You need to repair its derivation (see [Repair a derivation](#repair-a-derivation))
   1. Assumptions: 
      - current directory is `dir`
      - there is a `dir/flake.nix`
      - VSCodium is given as a derivation `codium`, like [here](https://github.com/br4ch1st0chr0n3/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L25)
   1. In `dir/flake.nix`, set `packages.default = codium;`, like [here](https://github.com/br4ch1st0chr0n3/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L37)
   1. Close:
      - devshells with this VSCodium
      - VSCodium itself
   1. `cd dir`
   1. Run `nix store repair .#`
   1. Open VSCodium. E.g., `nix run .#`
   1. Again, make a `Check` to verify binaries are on `PATH`
   1. If no, try restarting your OS
