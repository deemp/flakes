module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR


main :: Effect Unit
main = do
  log "🍝"

type Person = { "Name" ∷ String, age ∷ Int, "is active" ∷ Boolean }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { "Name": CA.string
      , age: CA.int
      , "is active": CA.boolean
      })