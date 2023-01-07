# manager

Manage `Haskell` projects with multiple `Main.hs` files. When creating such files, one's tasks can be:

- Make `Haskell Language Server` see all `Haskell` modules
- Invent new names for `stack`'s build targets
- Configure `ghcid` to run a specific function

`manager` simplifies these tasks.

Still, some actions like writing the correct dependencies and language extensions in `package.yaml` and `stack.yaml` should be done manually.

## Disclaimer

- `manager` satisfies my needs. It shouldn't necessarily satisfy yours
- This is an experimentation place for un-doable actions and accumulating exceptions into a monoid (described in [ExMonoid](src/ExMonoid.hs))
- It showcases a small `Haskell` project powered by `Nix`

## Prerequisites

- [Prerequisites](https://github.com/deemp/flakes#prerequisites)
- `VSCodium` for `Haskell` [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme)
- [template](./template/README.md)

## Quick start

Goal: initialize a new project via `manager`. Start `VSCodium` with extensions and executables for `Haskell`.

1. [Install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md)

1. Sometimes, Nix prints warnings and prompts in red color. Read them. Answer `y` if you agree or don't know. You'll be able to change the settings later.

1. Create a new project. There will be a devhshell with `VSCodium`, `manager`, and other tools:

```console
nix shell github:deemp/flakes/main?dir=manager
mkdir new-project
cd new-project
manager
manager init
direnv allow
nix develop
write-settings-json
codium .
```

1. Open `new-project/README.md` and follow instructions there

1. If you'd like to enable completions on `<TAB>` in your shell - see [this](https://github.com/pcapriotti/optparse-applicative#bash-zsh-and-fish-completions). With completions enabled, you'll be able to get `manager`'s list of commands:

  ```sh
  manager <TAB>
  --help  -h      add     init    list    rm      set     update
  ```

## Contribute

You can help improve `manager`

1. Clone the `flakes` [repo](https://github.com/deemp/flakes)
1. Open `VSCodium` and start development:

  ```console
  cd manager
  nix develop nix-dev/
  codium .
  # open a terminal in VSCodium
  ghcid
  ```

## References

- Dealing with exceptions - [src](http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html)

- Lenses - [src](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)
  - lens-aeson examples - [src](https://github.com/danidiaz/lens-aeson-examples/blob/master/src/Data/Aeson/Lens/Examples.hs)
  - more examples - [src](https://www.snoyman.com/blog/2017/05/playing-with-lens-aeson/)

- Safe Resource handling - [src](https://mmhaskell.com/blog/2022/6/23/resources-and-bracket)

- [managed](https://hackage.haskell.org/package/managed-1.0.9) package - with this package, we can collect into a monoid the exceptions that occur when doing and undoing actions
