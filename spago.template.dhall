{-
{{GENERATED_DOC}}

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
      , "transformers"
      , "validation"
      , "variant"
      ]
  -- This path is relative to config file
  , packages = {{PACKAGES_DIR}}/packages.dhall
  -- This path is relative to project root
  -- See https://github.com/purescript/spago/issues/648
  , sources = [ "{{SOURCES_DIR}}/src/**/*.purs", "{{SOURCES_DIR}}/test/**/*.purs" ]
  }
