{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210516/packages.dhall
        sha256:f5e978371d4cdc4b916add9011021509c8d869f4c3f6d0d2694c0e03a85046c8

let overrides = {=}

let additions =
      { option =
          { dependencies =
              [ "argonaut-codecs"
              , "argonaut-core"
              , "codec"
              , "codec-argonaut"
              , "either"
              , "foreign"
              , "foreign-object"
              , "lists"
              , "maybe"
              , "profunctor"
              , "prelude"
              , "record"
              , "simple-json"
              , "transformers"
              , "tuples"
              , "type-equality"
              , "unsafe-coerce"
              ]
          , repo = "https://github.com/joneshf/purescript-option.git"
          , version = "v9.0.0"
          }
      , pre =
          { dependencies =
              [ "aff"
              , "arrays"
              , "bifunctors"
              , "control"
              , "datetime"
              , "effect"
              , "either"
              , "enums"
              , "foldable-traversable"
              , "maybe"
              , "newtype"
              , "option"
              , "ordered-collections"
              , "parallel"
              , "prelude"
              , "profunctor-lenses"
              , "record"
              , "safe-coerce"
              , "transformers"
              , "tuples"
              , "variant"
              ]
          , repo = "https://github.com/citizennet/purescript-prelude.git"
          , version = "0ab411ff1d8dc58d55085eb92c244a9ca7e0f63c"
          }
      }

in  upstream // overrides // additions
