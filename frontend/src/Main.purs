module Main
  ( codec
  , main
  , p
  )
  where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show p."45tree"

type Person = { "Name" ∷ String, age ∷ Int, "45tree" ∷ Boolean }

p::Person
p = {"Name" : "hey", age : 3, "45tree" : true}

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { "Name": CA.string
      , age: CA.int
      , "45tree": CA.boolean
      })