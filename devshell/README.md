# devshell

Easily create a CLI to your `devShells`

- original `devshell` [repo](https://github.com/numtide/devshell)
- `devshell` [tutorial](https://yuanwang.ca/posts/getting-started-with-flakes.html#numtidedevshell)

This is a slightly modified `devshell`. It sets the default values for missing arguments of `commands`)

## Run default devshell

```sh
nix develop
```

## Components

See sample shell (`devShells.default`) in [flake.nix](./flake.nix)

`mkShell`:

- `packages` - binaries that will be available on `PATH`
- `bash`
  - `extra` - command that will be run on `devshell` start
- `commands` - list of command descriptions
  - `name` - command name. It gets a space character (`' '`) appended to avoid name clashes with original commands
  - `category` - category to group commands by
  - `help` - description of a command
  - `command` - a command to run
