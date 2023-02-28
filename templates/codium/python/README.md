# Python

- `VSCodium` with extensions and executables for `Python`.
- A sample `Python` project.

Feel free to remove the `VSCodium`-related `Nix` code and whatever you want!

## Prerequisites

- [flake.nix](./flake.nix) - code in this flake is extensively commented. Read it to understand how this flake works.
- [Conventions](https://github.com/deemp/flakes/blob/main/README/Conventions.md#dev-tools) - you may want to use this flake just for development.

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

## Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell and run the app.

    ```console
    nix flake new my-project -t github:deemp/flakes#codium-python
    cd my-project
    git init && git add
    nix develop
    python src/main.py
    ```

1. Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. Open a `Python` file `src/main.py` and hover over a function.

1. Wait until you see its docs.

1. [Run](https://code.visualstudio.com/docs/python/jupyter-support-py#_jupyter-code-cells) a code cell.

## Configs

- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
