# nix-managed

This is a Nix flake for trying Haskell. Its Haskell files are managed by `manager`, which uses the [managed](https://hackage.haskell.org/package/managed-1.0.9) package.

This flake contains:
- [VSCodium](https://vscodium.com/) with all necessary extensions for Haskell and Nix
- Shell tools for Haskell (`GHC 9.0.2`) and Nix, like `ghc`, `stack`, `ghcid`, `rnix-lsp`
- A hand-made tool for managing modules and file templates (`manager`)
- `stack.nix` which provides Nix packages from [flake.nix](flake.nix) (see `stack-shell` there)

## Quick start

- Install [Nix](https://nixos.org/download.html) (Single-user installation)
  ```sh
  sh <(curl -L https://nixos.org/nix/install) --no-daemon
  ```

- Enable [flakes](https://nixos.wiki/wiki/Flakes#Permanent). Create a file if missing

- Enter the repo
  ```sh
  git clone https://github.com/br4ch1st0chr0n3/acpoj
  cd acpoj
  ```

- Complete [direnv](https://direnv.net/docs/installation.html#from-system-packages) Installation

- Log out, Log in

- Allow direnv here
  ```sh
  direnv allow
  ```

- Now, when prompted, answer `y`

- This should load `haskell-language-server` and `stack` into shell. If no, run:
  ```
  nix develop
  ```

- This will load the shell tools and write `settings.json`. You can configure the necessary settings in `writeSettings` ([flake.nix](flake.nix))

- Okay, build the project to verify HLS can also build it
  ```
  stack build
  ```

- After that, run
  ```console
  nix develop .#codium
  ```

- A Codium instance with the promised tools should open.

- Learn about `manager`'s supported commands
  ```sh
  manager --help
  ```

- Or, see the documentation for a specific command
  ```sh
  manager add --help
  ```

- Now, create a module:
  ```sh
  manager add B
  ```

- Autocomplete may work for you when pressing the `TAB` button. Remove this module
  ```sh
  $ manager <TAB>
  --help    -h        add       list      rm        set       template
  $ manager rm B
  Removing './Modules/B.hs'
  Reading './package.yaml'
  Updating './package.yaml'
  Updating .cabal
  generated nix-managed.cabal
  Done!
  ```

- When you open the newly created file, `./Modules/B.hs`, and hover over a term, you should see Haskell Language Server load and show info.

- In case of problems, try to
  - Open `Command Palette`: `Ctrl` (`Cmd`) + `Shift` + `P`
    - Restart HLS: `Command Palette` > `Restart Haskell LSP Server`
    - Reload the window: `Command Palette` > `Reload Window`

- Feel free to create an issue or contact me at [Telegram](https://daniladanko.t.me)

## Miscellaneous

* Dealing with exceptions - [src](http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html)

* Lenses - [src](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)
  * lens-aeson examples - [src](https://github.com/danidiaz/lens-aeson-examples/blob/master/src/Data/Aeson/Lens/Examples.hs)
  * more examples - [src](https://www.snoyman.com/blog/2017/05/playing-with-lens-aeson/)

* Safe Resource handling - [src](https://mmhaskell.com/blog/2022/6/23/resources-and-bracket)

* [managed](https://hackage.haskell.org/package/managed-1.0.9) package - with this package, we can collect into a monoid the exceptions that occur when doing and undoing actions. See [manager](./manager/)