# lhsc - a prepocessor for literate haskell markdown

A Converter to convert from .lhs to .md and vice versa

`lhsc` is a program to convert literate haskell files in the 
birdtick format to correctly rendered (git flavoured, html containing) markdown files 
and vice versa.

It strips away the heading tags #, replacing them with the corresponding html tags,
converts the `'''haskell [...]'''` to `> [...]` as recognised by the GHC literate prepocessor.
`''' [...] '''` will be converted to `< [...]` and will be discarded by ghc but will still be displayed as code when rendered. 
(In both cases `'''` is actually the three md backticks, but its a pain to write md about md.)

# Usage

`lhsc (toLhs|toMd) file1 [file2] [...]`
The `toLhs` and `toMd` commands are not case sensitive. 
The program will convert each file from the other format to the specified one, 
creating the files `file1.md` `file2.md` ... or `file1.lhs` `file2.lhs` ... in the same directory respectively.

To install the executeable on windows, if you can't convince cabal to use [`--bindir-method=copy`](https://github.com/haskell/cabal/issues/5748) you can build the project locally and copy the built executeable to `C:/Users/username/AppData/Roaming/cabal/bin` and ensure that this directory is in your path. 
