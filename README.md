# Codium for Haskell

This is a Nix flake for Haskell development or problem-solving. It can be used for OJ, e.g., ACPOJ (hosted by @ParfenovIgor).

It contains:
- Codium with all necessary extensions for Haskell and Nix
- Shell tools for Haskell and Nix, like ghc, stack, ghcid
- A hand-made tool for adding and removing problems (template file to be supported)

## Quick start

- Install [Nix](https://nixos.org/download.html) (Single-user installation)
  ```sh
  sh <(curl -L https://nixos.org/nix/install) --no-daemon
  ```

- Enable [flakes](https://nixos.wiki/wiki/Flakes#Permanent)

- Enter the repo
  ```sh
  git clone https://github.com/br4ch1st0chr0n3/acpoj
  cd acpoj
  ```

- Install [direnv](https://nix.dev/tutorials/declarative-and-reproducible-developer-environments#direnv-automatically-activating-the-environment-on-directory-change) - steps 1, 2

- Allow direnv here
  ```sh
  direnv allow
  ```

- Now, everything should load. You may have to wait

- After that, run
  ```sh
  codium .
  ```

- A Codium instance with all promised tools should open

- Try to add a problem:
  ```sh
  problem add F
  ```

- Or remove it
  ```sh
  problem rm F
  ```

- When you open a problem file (e.g. `A.hs`), you should see Haskell Language Server load and show info when you hover over a term.

- In case of problems, try to reload the window (`Ctrl` + `Shift` + `P` > `Reload Window`)