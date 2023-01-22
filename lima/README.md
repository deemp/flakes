# lima

- Convert `Haskell` (`.hs`) files with `Markdown` comments to `Markdown` (`.md`)
- Convert between `Literate Haskell` (`.lhs`) files  and `Markdown` (`.md`).

For `lhs2md` -> `md2lhs` conversion, `lima` usually abides the `round-trip property`. In other words if its chain of conversions is `file.lhs` -> `file.lhs.md` -> `file.lhs.md.lhs`, then `file.lhs = file.lhs.md.lhs` in terms of their contents. Such naming is because `lima` creates a new file named same as the file to convert plus a relevant suffix.

## Background

This is a fork of [LiterateMarkdown](https://github.com/haskie-lambda/LiterateMarkdown). Initially, I just wanted to fix some bugs, but then realized that I can't conveniently use Haskell Language Server with `.lhs` files so I added the `.hs` -> `.md` conversion.

### file.hs -> file.hs.md

Examples:

- [input2.hs](./testdata/input2.hs) -> [input2.hs.md](./testdata/input2.hs.md)

Rules:

- Write comments in `GitHub`-flavoured `Markdown`
- Magic comments like `{- FOURMOLU_ENABLE -}` will be ignored
- You can ignore parts of a file via `{- LIMA_DISABLE -}` and `{- LIMA_ENABLE -}`
- Magic comments can't be used in snippets

### file.lhs -> file.lhs.md -> file.lhs.md.lhs

Examples:

- [input0.lhs](./testdata/input0.lhs) -> [input0.lhs.md](./testdata/input0.lhs.md)
- [input1.lhs](./testdata/input1.lhs) -> [input1.lhs.md](./testdata/input1.lhs.md)

As `.lhs` doesn't support `#` (heading) or `>` (quotation start) at a line beginning, one should write ` #` and ` >` instead.

- ` #` -> `#` -> ` #`
- ` >` -> `>` -> ` >`

If you'd like to provide some code in a `.lhs`, follow these rules:

- `>` is for `Haskell` code, there should be an empty line before and after the block with `Haskell` code
- `<` is for any other code. Such code will be converted into code blocks of type `console` in `.md`
- The round-trip property is not guarranteed if you insert code snippets into `.lhs` using three backticks
  - Nevertheless, feel free to insert them into `.md`. In `.lhs`, they will just lose the language info

## Usage

`lima (md2lhs|lhs2md|hs2md) file1 [file2] [...]`
The `md2lhs`, `lhs2md`, `hs2md` commands are case-insensitive.
The tool will convert each file from the one format to the specified one, creating the files `file1.md` `file2.md` ... or `file1.lhs` `file2.lhs` ... in the same directory respectively.

To install the executable on Windows, if you can't convince cabal to use [`--bindir-method=copy`](https://github.com/haskell/cabal/issues/5748) you can build the project locally and copy the built executeable to `C:/Users/username/AppData/Roaming/cabal/bin` and ensure that this directory is in your `PATH`.

## Contribute

1. Learn about `Nix` - [src](https://github.com/deemp/flakes#prerequisites)
1. Open `VSCodium`:

    ```sh
    nix develop nix-dev/
    write-settings-json
    codium .
    ```

1. Open a Haskell file there, hower over a term and wait until `HLS` shows the hints
