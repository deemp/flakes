module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


main :: Effect Unit
main = do
  log "🍝"

tree :: String
tree = "tree"

type Person = { "Name" ∷ String, age ∷ Int, tree ∷ Boolean }

p::Person
p = {"Name" : "hey", age : 3, "tree" : true}

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { "Name": CA.string
      , age: CA.int
      , "tree": CA.boolean
      })