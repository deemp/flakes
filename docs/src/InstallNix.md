# Install Nix

1. Install [Nix](https://nixos.org/download.html) (Single-user installation)

    ```console
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    ```

1. [Enable](https://nixos.wiki/wiki/Flakes#Permanent) flakes
1. Set `show-trace = true` ([doc](https://nixos.org/manual/nix/unstable/command-ref/conf-file.html#conf-show-trace)) to see more complete error messages.

## Community

- Join `NixOS` [community](https://nixos.org/community/)
  - [Telegram](https://t.me/ru_nixos)
- [Discourse](https://discourse.nixos.org/)

## Nix messages

Sometimes, when you enter a devshell (e.g., `nix develop`) or run a default package of a flake, you may see:

- warnings - not a problem, just read them and google if you're interested;
- errors - the same story;
- prompts - answer `y` (the simplest way).

## direnv

This is a tool for running scripts when you `cd` to a directory containing a `.envrc` file.
`direnv` automatically builds, caches, and starts a devshell when you enter a flake directory containing the `.envrc` file (e.g., the root directory of this repository).
See [Direnv integration](https://nixos.wiki/wiki/Flakes#Direnv_integration).

1. Install `direnv` - [src](https://direnv.net/docs/installation.html#installation)
    1. Install the binary

        ```console
        nix profile install nixpkgs#direnv
        ```

    1. [Hook](https://direnv.net/docs/hook.html) into your devshell

1. Allow `direnv` to work in a directory.

    ```console
    direnv allow
    ```

    When you see `direnv` errors, run the suggested commands.

## Further reading

See [Nix prerequisites](./NixPrerequisites.md).
