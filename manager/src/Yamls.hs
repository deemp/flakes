{-# LANGUAGE QuasiQuotes #-}

module Yamls (initPackageYaml, initStackYaml) where

import Text.RawString.QQ (r)
import Data.ByteString (ByteString)

initPackageYaml :: ByteString
initPackageYaml =
  [r|
default-extensions:
- TypeApplications

dependencies:
- base >= 4.7 && < 5

executables: {}

ghc-options:
- -Wall

license: BSD3

name: nix-managed

version: 0.1.0.0
|]

initStackYaml :: ByteString
initStackYaml =
  [r| 
resolver: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/32.yaml

ghc-options:
  "$everything":
    -Wno-missing-signatures
    -Wno-unused-top-binds
    -haddock
packages:
  - .
extra-deps:
  - sockets-and-pipes-0.3
|]