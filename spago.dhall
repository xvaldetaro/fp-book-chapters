{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "contravariant"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "functions"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "random"
  , "semirings"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "undefined"
  , "unfoldable"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
