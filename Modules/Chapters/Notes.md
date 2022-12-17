# Notes

## C_2_1

- `Text` is strict

## C_3_1

- Can pack octets `01101000 -> 104`
- `Word8, Word16, Word32, Word64`

## C_3_3

- `UTF-8` and `UTF-16` are variable-width encodings. They use indicator bits to inform if the subsequent bytes are part of a curernt character.
- `HTTP` uses `ASCII`
- Some parts of `HTTP` use `UTF-8`
- There may be decoding and encoding errors depending on character encodings and character sets
- To write text as bytes, you must choose an encoding
- There are several functions to convert btw `Text` and `ByteString`
- `Unicode` character example: `ord '♫'`
- add a package to `GHCi`:

    ```sh
    ghci> :set -package base
    package flags have changed, resetting and loading new packages...
    ghci> :m +Data.C
    Data.Char     Data.Coerce   Data.Complex
    ghci> :m +Data.Char
    ghci> ord '♫'
    9835
    ```

## C_3_4

- It's possible in `GHCi` to set a flag and show the type of evaluated expressions: `:set +t`
- `Show` instance for `ByteString` is primitive. It's dangerous to have functions depend on the `show` outputs
- `OverloadedStrings` use `fromString`, and this function can also be incomplete
- It's safe to use `OverloadedStrings` only with `Text` and `String` as they're almost the same
- Libraries may not let you know about that
- The book won't use `OverloadedStrings`

## C_4_1

- Unix domain (`AF_UNIX`) is for inter-process communication

## C_4_4

- `SockAddr` - if know protocol and socket type
- `AddrInfo` - `SockAddr` + info about protocol
- `getAddrInfo` == `host` - look up a name and IP address

## C_5_3

- Header lines should end with `\r\n`

## C_5_4

- Body lines should not end with `\r\n`
- `lift` - lift a character set into its superset - [ASCII.Lift](https://hackage.haskell.org/package/ascii-superset-1.0.1.13/docs/ASCII-Lift.html)

## C_6_5

- A lazy `ByteString` is like a lazy list of strict `ByteString`s

## C_7_2

- speed testing - `criterion`
- `Data.Text.Lazy.Builder` significantly speeds up building large text from chunks as compared to strict `Text`
- `fold` and `foldMap`

## C_8_2

- `http-types` - library with common types and expressions for http message construction
- `LBS.length` gives an `Int64`

## C_8_3

- `Integer` - unbounded integers
- `Int<n>` - integer with a given number of bits, like `Int64`
- `Int` - `2^29 - 1` or more (depends)
- Length of a laazy bytestring can be `2^64` bytes
- `LBS.length :: LBS.ByteString -> Int64`

## C_9_4

- `MultiWayIf` - language extension, if with guards - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/multiway_if.html)

## C_10_1

- Sockets connect machines; files connect processes; `TVar`s connect threads.

## C_10_5 

- `stm-containers` - `TVar` - based hash map