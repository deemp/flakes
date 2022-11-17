# Install

## Nix

1. Install [Nix](https://nixos.org/download.html) (Single-user installation)

    ```console
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    ```

1. Enable [flakes](https://nixos.wiki/wiki/Flakes#Permanent). Create a file if missing

1. Log out, Log in to Linux

### devShells

Sometimes, when you enter a devshell:

```console
nix develop
```

you may see:

- warnings - this is not a problem
- prompts - answer `y`

## direnv

1. Complete [direnv](https://direnv.net/docs/installation.html#installation) Installation
    1. To complete the first part:

        ```console
        nix profile install nixpkgs#direnv
        ```
