{-
The `formlet` package contains the core definitions for the `formlet` abstraction.
Definitions of specific forms and their rendering implementations do not belong
in this package.
-}
let
  name = "formlet"
in
  { name
  , dependencies =
      [ "arrays"
      , "const"
      , "control"
      , "either"
      , "foldable-traversable"
      , "identity"
      , "integers"
      , "newtype"
      , "numbers"
      , "option"
      , "ordered-collections"
      , "pre"
      , "prelude"
      , "profunctor"
      , "profunctor-lenses"
      , "quickcheck"
      , "record"
      , "strings"
      , "test-unit"
      , "test-utils"
      , "transformers"
      , "validation"
      , "variant"
      ]
  , packages = ../../packages.dhall
  -- Due to a spago bug (see https://github.com/purescript/spago/issues/648)
  -- `sources` are relative to root instead of config file.
  , sources = [ "lib/${name}/src/**/*.purs", "lib/${name}/test/**/*.purs" ]
  }
