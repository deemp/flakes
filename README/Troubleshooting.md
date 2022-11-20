# Troubleshooting

## Prerequisites

See [Prerequisites](Prerequisites.md)

## Substituters and keys

There are `extra-trusted-public-keys`, `extra-trusted-public-keys` (like [here](https://github.com/br4ch1st0chr0n3/flakes/blob/7bd58c9cf9708714c29dadd615d85d22ded485ae/flake.nix#L112)). If a substituter like `cachix` fails, comment out the lines containing its address

## Repair a derivation

[Derivation](https://nixos.org/manual/nix/unstable/language/derivations.html?highlight=derivation#derivations)

Repair a derivation - [manual](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-store-repair.html)

Alternative steps:

   1. Assumptions:
      - current working directory contains `flake.nix`
      - your corrupt derivation is available inside this `flake.nix` by the name `your-corrupt-derivation`
   1. Set `packages.default = your-corrupt-derivation` in this `flake.nix`
   1. Run `nix store repair .#`
      - `.#` denotes an [installable](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix.html?highlight=installable#installables)

## VSCodium troubleshooting

### GitHub Personal Access Token (PAT) for VS Codium extensions

- Create a `classic` PAT with permissions: `read:user, repo, user:email, workflow`
- Supply it to extensions

### Missing binaries on PATH in VSCodium

Case: VSCodium doesn't have the binaries provided in `runtimeDependencies` (like [here](https://github.com/br4ch1st0chr0n3/flakes/blob/7bab5d96658007f5ad0c72ec7805b5b4eb5a83dd/templates/codium/generic/flake.nix#L33)) on `PATH`:

   1. You need to repair VSCodium's derivation (see [Repair a derivation](#repair-a-derivation))
   1. Assumptions:
      - current directory is `DIR`
      - there is a `DIR/flake.nix`
      - VSCodium is given as a derivation `codium`, like [here](https://github.com/br4ch1st0chr0n3/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L25)
   1. In `DIR/flake.nix`, set `packages.default = codium;`, like [here](https://github.com/br4ch1st0chr0n3/flakes/blob/53b2e4d8bb5fb34c50da1b45f06622bffdb9b7bf/templates/codium/generic/flake.nix#L37)
   1. `Check`:
      1. `cd DIR`
      1. Start VSCodium: `nix run .#`
      1. Open a VSCodium terminal
      1. `echo $PATH` there
      1. It doesn't contain `/bin` dirs of specified `runtimeDependencies`
   1. Close:
      - devshells with this VSCodium
      - VSCodium itself
   1. Open a new terminal, `cd DIR`
   1. Run `nix store repair .#`
   1. Make a `Check` to verify binaries are on `PATH`
   1. If still no, continue
   1. Remove direnv profiles:
      - `cd DIR && rm -rf .direnv`
   1. Restart your OS
   1. `nix store gc` - collect garbage in Nix store - [man](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-store-gc.html)
   1. Again, make a `Check`
