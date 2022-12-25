# lhsc

Convert between `.lhs` (`Literate Haskell` files) and `.md` (`Markdown`).

`lhsc` abides the `round-trip property`. It converts `file.lhs` -> `file.lhs.md` -> `file.lhs.md.lhs` and usually guarrantees `file.lhs = file.lhs.md.lhs` in terms of their contents.

## Transformations

See [testdata](./testdata/) for conversion examples. They're generated using `cabal test`.

### file.lhs -> file.lhs.md -> file.lhs.md.lhs

As `.lhs` doesn't support `#` (heading) or `>` (quotation start) at a line beginning, one should write ` #` and ` >` instead.

- ` #` -> `#` -> ` #`
- ` >` -> `>` -> ` >`

If you'd like to provide some code in `.lhs`, follow these rules:

- `>` is for `Haskell` code, there should be an empty line before and after the block with `Haskell` code
- `<` is for any other code. Such code will be converted into code blocks of type `console` in `.md`
- The round-trip property is not guarranteed if you insert code snippets into `.lhs` using three backticks
  - Nevertheless, feel free to insert them into `.md`. In `.lhs`, they will just lose the language info

## Usage

`lhsc (toLhs|toMd) file1 [file2] [...]`
The `toLhs` and `toMd` commands are case-insensitive.
The tool will convert each file from the one format to the specified one, creating the files `file1.md` `file2.md` ... or `file1.lhs` `file2.lhs` ... in the same directory respectively.

To install the executable on Windows, if you can't convince cabal to use [`--bindir-method=copy`](https://github.com/haskell/cabal/issues/5748) you can build the project locally and copy the built executeable to `C:/Users/username/AppData/Roaming/cabal/bin` and ensure that this directory is in your `PATH`.

## Contribute

1. Learn about `Nix` - [src](https://github.com/deemp/flakes#prerequisites)
1. `nix develop` to get `cabal` on `PATH`
