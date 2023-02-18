# lima

Convert between

- `Haskell` (`.hs`) and `GitHub Flavored Markdown` (`.md`)
- `Literate Haskell` (`.lhs`) and `GitHub Flavored Markdown` (`.md`).

It is usually possible to make conversion abide the [roundtrip property](https://jesper.sikanda.be/posts/quickcheck-intro.html). In other words, make conversions `file.lhs` -> `file.lhs.md` -> `file.lhs.md.lhs` or `file.hs` -> `file.hs.md` -> `file.hs.md.hs` and get `file.lhs = file.lhs.md.lhs` and `file.hs = file.hs.md.hs` in terms of their contents.

## Demo

![demo](https://github.com/deemp/flakes/blob/main/lima/README/demo.png?raw=true)

## Alternatives

- [LiterateMarkdown](https://github.com/haskie-lambda/LiterateMarkdown). `lima` is a fork of this (abandoned?) project. Initially, I just wanted to fix some bugs, but then realized that I can't conveniently use `Haskell Language Server` with `.lhs` files so I added the `.hs` -> `.md` conversion.

- [IHaskell](https://github.com/IHaskell/IHaskell) - create `Jupyter` notebooks with `Haskell` code cells and `GitHub Flavored Markdown` text cells and do much more!

## Conversion

### .hs -> .md

#### Examples

1. [hs](./testdata/hs/input0.hs) -> [hs.md](./testdata/hs/input0.hs.md) -> [hs.md.hs](./testdata/hs/input0.hs.md.hs)

#### Rules

- **Multiline comments**:
  - Become text blocks.
  - Should be written in `GitHub Flavored Markdown`.
  - Should start with `{- ` or `{-\n`.
  - Split `Haskell` code into parts above a comment and below a comment.
- **Special comments**:
  - Become comments
  - Can be supplied in a config via `lima --config <your config path>` ([sample config](./testdata/config/))
  - Should be written in a single line
  - `{- LIMA_INDENT N -}` increases the indentation of snippets by `N`
  - `{- LIMA_DEDENT -}` sets the indentation of snippets to `0`
  - `{- LIMA_DISABLE -}` starts copying the following lines verbatim
  - `{- LIMA_ENABLE -}` stops copying the following lines verbatim
- **Code and single-line comments**:
  - Become `hs` snippets

### .lhs -> .md

#### Examples

1. [hs](./testdata/lhs/input0.lhs) -> [hs.md](./testdata/lhs/input0.lhs.md) -> [hs.md.hs](./testdata/lhs/input0.lhs.md.lhs)
1. [hs](./testdata/lhs/input1.lhs) -> [hs.md](./testdata/lhs/input1.lhs.md) -> [hs.md.hs](./testdata/lhs/input1.lhs.md.lhs)

#### Rules

- **Text**:
  - As `.lhs` doesn't support `#` (heading) or `>` (quotation start) at a line beginning, write ` #` and ` >` instead. `lhs` -> `lhs.md` -> `lhs.md.lhs`:

    - <code>&nbsp;#</code> -> `#` -> <code>&nbsp;#</code>
    - <code>&nbsp;></code> -> `>` -> <code>&nbsp;></code>
- **Snippets**:

  - `>` is for `Haskell` code. There should be an empty line before and after the block with `Haskell` code
  - `<` is for any other code. Such code will be converted into code blocks of type `console` in `.md`
  - Snippets in <code>\`\`\`</code> become <code>\`\`\`console</code> and then `<`.
    - The round-trip property is not guarranteed

## Command-line tool

### From Hackage

1. Install via cabal

    ```console
    cabal update
    cabal install lima
    ```

### From sources

1. Clone this repo and install `lima`.

    ```console
    git clone https://github.com/deemp/flakes
    cd flakes/lima
    cabal update
    cabal install .
    ```

### Nix

1. [Install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md)

1. Get `lima` on `PATH`.

    ```console
    nix flake lock github:deemp/flakes?dir=lima
    nix shell github:deemp/flakes?dir=lima
    lima --help
    ```

### Windows

To install the executable on `Windows`, if you can't convince cabal to use [`--bindir-method=copy`](https://github.com/haskell/cabal/issues/5748) you can build the project locally and copy the built executeable to `C:/Users/username/AppData/Roaming/cabal/bin` and ensure that this directory is in your `PATH`.

## build-tool-depends

You can use `lima` to generate your docs, e.g., via `cabal test:docs`. Just provide such a test with a script that converts (and, possibly, combines) files. As you'll use `lima` in a script, you should add it to that test's `build-tool-depends`:

  ```cabal
  build-tool-depends:
      lima:lima ==0.1.*
  ```

### Nix flake

1. Add `lima` to inputs:
  
  ```nix
  inputs.lima.url = "github:deemp/flakes?dir=lima";
  ```

1. Add `lima` to the override of your package

  ```nix
  override = {
    overrides = self: super: {
      myPackage = overrideCabal
        (super.callCabal2nix myPackageName ./. { })
        (x: {
          testHaskellDepends = [
            (super.callCabal2nix "lima" "${lima.outPath}/lima" { })
          ] ++ (x.testHaskellDepends or [ ]);
        });
    };
  };
  ```

1. Use `cabal v1-test` so that `cabal` uses the supplied `lima`.

## Contribute

Clone this repo and enter `lima`

```console
git clone https://github.com/deemp/flakes
cd flakes/lima
```

### cabal

Build as usually

```console
cabal update
cabal build
```

### nix

1. [Install](https://github.com/deemp/flakes/blob/main/README/InstallNix.md) `Nix`.

1. Run a devshell and build `lima`:

    ```console
    nix develop nix-dev/
    cabal build
    ```

1. Optionally, start `VSCodium`:

    ```console
    nix run nix-dev/#writeSettings
    nix run nix-dev/#codium .
    ```

1. Open a `Haskell` file there, hover over a term and wait until `HLS` shows hints.

1. [Troubleshoot](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md) if necessary.
