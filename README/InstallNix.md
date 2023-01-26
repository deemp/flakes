# Install

## Nix

1. Edit [nix.conf](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#description)
    1. Create file if missing
    1. [Enable](https://nixos.wiki/wiki/Flakes#Permanent) flakes in it
    1. Set `show-trace = true` ([doc](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#conf-show-trace))

1. Install [Nix](https://nixos.org/download.html) (Single-user installation)

    ```console
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    ```

## Community

- Join `NixOS` [community](https://nixos.org/community/)
  - [Telegram](https://t.me/ru_nixos/19843)

## Nix messages

Sometimes, when you enter a devshell (e.g., `nix develop`) or run a default package of a flake, you may see:

- warnings - not a problem, just read them and google
- errors - the same story
- prompts - answer `y` (the simplest way)

## direnv

This is a tool for running scripts in `.envrc` files when you enter a directory containing such file. One of its usages with flakes is to automatically build, cache, and enter a devshell when you enter that flake's directory in a terminal. See [Direnv integration](https://nixos.wiki/wiki/Flakes#Direnv_integration)

1. Install `direnv` - [src](https://direnv.net/docs/installation.html#installation)
    1. Install the binary

        ```console
        nix profile install nixpkgs#direnv
        ```

    1. [Hook](https://direnv.net/docs/hook.html) into your devshell

When you see `direnv` errors, run the suggested commands
