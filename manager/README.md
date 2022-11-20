# manager

Manage repetitive `Haskell` modules and templates.

Sometimes, one wants to experiment with `Haskell` in multiple unrelated files. When creating such files, one's tasks can be:

- Make `Haskell Language Server` see all `Haskell` modules
- Invent new names for `stack`'s build targets
- Configure `ghcid` to run a specific function

`manager` simplifies these tasks.

Still, some actions like writing the correct dependencies and language extensions in `package.yaml` and `stack.yaml` should be done manually.

## Prerequisites

- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- [VSCodium template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell)
- [template](./template/README.md)

## Usage

Goal: initialize a new project via `manager`. Start `VSCodium` with extensions and executables for `Haskell`.

1. Create a new project. `flake.nix` there will provide `VSCodium`, `manager`, and other tools in a devshell:

```console
nix shell github:deemp/flakes/main?dir=manager
mkdir new-project
cd new-project
manager
# answer `y` if prompted
manager init
direnv allow
nix develop
write-settings-json
codium .
```

1. Open `README.md` and follow instructions

## Contribute

You can help improve `manager`

1. Clone the `flakes` [repo](https://github.com/deemp/flakes)
1. Open `VSCodium` and start development:

  ```console
  cd manager
  nix develop nix-dev/
  codium .
  ```

## Miscellaneous

- Dealing with exceptions - [src](http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html)

- Lenses - [src](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)
  - lens-aeson examples - [src](https://github.com/danidiaz/lens-aeson-examples/blob/master/src/Data/Aeson/Lens/Examples.hs)
  - more examples - [src](https://www.snoyman.com/blog/2017/05/playing-with-lens-aeson/)

- Safe Resource handling - [src](https://mmhaskell.com/blog/2022/6/23/resources-and-bracket)

- [managed](https://hackage.haskell.org/package/managed-1.0.9) package - with this package, we can collect into a monoid the exceptions that occur when doing and undoing actions. See [manager](./manager/)
