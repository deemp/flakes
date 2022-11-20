# Install

## Nix

1. Install [Nix](https://nixos.org/download.html) (Single-user installation)

    ```console
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    ```

1. Edit [nix.conf](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description)
    - Create file if missing
    - [Enable](https://nixos.wiki/wiki/Flakes#Permanent) flakes
    - Set [show-trace](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#conf-show-trace) = true

1. Log out, log in to your OS

### devshells

Sometimes, when you enter a devshell (e.g., `nix develop`), you may see:

- warnings - not a problem, just read them and google
- errors - the same story
- prompts - answer `y` (the simplest way)

## direnv

This is a tool for running scripts in `.envrc` files when you enter a directory containing such file

1. Complete [direnv](https://direnv.net/docs/installation.html#installation) installation
    1. To complete the first part:

        ```console
        nix profile install nixpkgs#direnv
        ```

When you see `direnv` errors, run the suggested command
