{ name = "purescript-pipeline"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "custom-prelude"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "freet"
  , "functions"
  , "functors"
  , "identity"
  , "newtype"
  , "parallel"
  , "prelude"
  , "spec"
  , "tailrec"
  , "these"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
