# Haskell project with `manager`

## Prerequisites

- See `VSCodium` for `Haskell` [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme).
It explains what's available in this project.
- Next, see Haskell [Prerequisites](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Haskell.md).
- Also, read [manager](https://github.com/deemp/flakes/tree/main/manager#readme) docs.
- Recurse into `Prerequisites` to get even more info.

## Quick start

1. If you haven't yet started `VSCodium` from this flake:

    ```terminal
    nix develop
    # if you haven't yet written 
    # or have already changed the settings
    write-settings-json
    codium .
    ```

1. Open a `Haskell` file `Modules/B/Main.hs` and hover over a function. `Haskell Language Server` should start giving you type info.

1. Add `manager` to `flake.nix`.
   1. Add `inputs.manager.url = github:deemp/flakes?dir=manager;`
   1. Add `manager` to the argument set of outputs
   1. Add `manager.packages.${system}.default` to the `codiumTools` list

1. Open a terminal in `VSCodium`. Run commands there.

    ```console
    nix flake update manager
    nix develop
    manager
    ```

1. Run the `main` function in `Modules/B/Main.hs` using `ghcid`:

    ```terminal
    manager set B/Main main
    ghcid
    ```

1. Run the `someFunction` function in `Modules/B/AnotherModule.hs` using `ghcid`:
    1. Stop the previous `ghcid` process: `Ctrl` (`Cmd`) + `C`
    2. Start it with a new configuration

        ```terminal
        manager set B/AnotherModule someFunction
        ghcid
        ```

1. See what are the available `executables`:

    ```terminal
    manager list
    ```

1. Copy an `executable` `A` to produce an `executable` `C`.
    1. Use `manager`:

        ```terminal
        manager add C A
        ```

    1. Check that `HLS` works: hover over a function in `Modules/C/Main.hs`.

1. Now, add a new module `CModule.hs` to `Modules/C`.
    1. Copy an existing module:

        ```terminal
        cp Modules/B/AnotherModule.hs Modules/C/CModule.hs
        ```

    1. Next, make `stack` and `HLS` aware of it:

        ```terminal
        manager update
        ```

1. Okay, load this new module into `stack repl`:

    ```terminal
    stack repl Modules/C/CModule.hs
    ```

1. Now, delete the `C` `executable`. Anyway, you know how to recreate it:

    ```terminal
    manager rm C
    ```

1. If you `manager rm` all `executables`, you'll have to manually add your new `executables`. See `Haskell` [Prerequisites](#prerequisites) then.

## Troubleshooting

See [Troubleshooting](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Troubleshooting.md).
