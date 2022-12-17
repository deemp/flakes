{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "argonaut-codecs"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "maybe"
  , "newtype"
  , "prelude"
  , "purescript-argonaut-generic"
  , "web-socket"
  , "either"
  , "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "css"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "media-types"
  , "ordered-collections"
  , "purescript-web-url"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "control"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
