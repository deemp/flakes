# VSCodium

Several templates for setting up `VSCodium` as a flake.

They're based on [codium](https://github.com/br4ch1st0chr0n3/flakes/tree/main/codium) flake.

## Example

Based on [Dev tools conventions](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Conventions.md#dev-tools)

```console
mkdir new-project
cd new-project
git init
nix flake new nix-dev -t github:br4ch1st0chr0n3/flakes#codium-generic
git add .
git commit -m "add flake"
nix develop nix-dev/
codium .
```
