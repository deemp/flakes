# VSCodium generic

This flake provides a devshell with `VSCodium` a `hello` executable on `PATH` and with a couple of extensions.

## Prerequisites

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [nix-vscode-extensions](https://github.com/nix-community/nix-vscode-extensions) (pinned [here](https://github.com/deemp/flakes/blob/main/source-flake/vscode-extensions/flake.nix)).
- [Prerequisites](https://github.com/deemp/flakes#prerequisites).
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)

## Example

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, run `VSCodium` from a devshell:

```console
nix flake new my-project -t github:deemp/flakes#codium-generic
cd my-project
git init && git add
nix develop
hello
```

1. Write `settings.json` and start `VSCodium`

```console
nix run .#writeSettings
codium .
```

1. If necessary, add other extensions
