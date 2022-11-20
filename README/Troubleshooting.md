# Troubleshooting

## Prerequisites

See [Nix Prerequisites](NixPrerequisites.md)

## Substituters and keys

There are `extra-trusted-public-keys`, `extra-trusted-public-keys` (like [here](https://github.com/deemp/flakes/blob/7bd58c9cf9708714c29dadd615d85d22ded485ae/flake.nix#L112)). If a substituter like `cachix` fails, comment out the lines containing its address

## Repair a derivation

Repair a derivation - [manual](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-store-repair.html)

Alternative steps:

   1. Assumptions:
      - current working directory contains `flake.nix`
      - your corrupt derivation is available inside this `flake.nix` by . name `your-corrupt-derivation`
   1. Set `packages.default = your-corrupt-derivation` in this `flake.nix`
   1. Run `nix store repair .#`
      - `.#` denotes an [installable](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix.html?highlight=installable#installables)

## VSCodium troubleshooting

See [VSCodium troubleshooting](https://github.com/deemp/flakes/blob/main/codium/README.md#troubleshooting)
