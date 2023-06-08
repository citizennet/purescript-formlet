module Formlet.Render.Forest
  ( Forest
  , Tree(..)
  , leaf
  , mapKey
  , node
  ) where

import CitizenNet.Prelude

import Formlet as Formlet
import Formlet.Options as Formlet.Options
import Formlet.Render as Formlet.Render
import Formlet.Render.List as Formlet.Render.List
import Option as Option
import Prim.RowList as Prim.RowList

-- | A `Forest` is a `List` of `Tree`s, and, as such, when used as the render
-- | functor of a Form, enables the combination of Forest-shaped forms.
type Forest options renders =
  Formlet.Render.List.List (Tree options renders)

-- | A Form Render functor that represents a Rose-Tree-shaped form.
-- |
-- | Since this Render functor should be the most used one, for the sake of
-- | ergonomics and dev experience, we have embedded the canonical,
-- | `VariantF`-based `Formlet.Render.Render` into the leaves, otherwise we'd end
-- | up with way too large render types.
data Tree options renders action
  = Leaf (Formlet.Render.Render options renders action)
  | Node
      { options :: Option options
      , children :: Forest options renders action
      }

derive instance functorTree :: Functor (Tree options renders)

-- When dealing with `Node` trees, the `HasOptions` instance for `Tree` only
-- reads and modifies the options of that `Node`, instead of propagating any
-- changes to the children.
instance hasOptionsTree :: Formlet.Options.HasOptions options (Tree options renders) where
  get label = case _ of
    Leaf l -> Formlet.Options.get label l
    Node n -> Option.get label n.options
  set label value = case _ of
    Leaf l -> Leaf (Formlet.Options.set label value l)
    Node n -> Node n { options = Option.set label value n.options }

-- | Embed a singleton Form into a `Tree`-shaped Form while also setting the
-- | specified options. E.g.:
-- |
-- | ```purescript
-- | testForm :: forall config options renders m. Form config (Render ( name :: String, description :: String | options ) renders) m String String
-- |
-- | field1 :: forall config options render m. Form config (Tree ( name :: String, description :: String | options ) renders) m String String
-- | field1 = leaf { name: "Test" } testForm
-- |
-- | field2 :: forall config options render m. Form config (Tree ( name :: String, description :: String | options ) renders) m String String
-- | field2 = leaf {} testForm
-- | ```
-- | a record with a subset
leaf ::
  forall config options list record renders m value result.
  Prim.RowList.RowToList record list =>
  Formlet.Options.SetOptions list record options (Tree options renders) =>
  Record record ->
  Formlet.Form config (Formlet.Render.Render options renders) m value result ->
  Formlet.Form config (Tree options renders) m value result
leaf record = Formlet.mapRender (Formlet.Options.set' record <<< Leaf)

-- | Recursively apply the given function to all keys in a `Forest`.
mapKey ::
  forall options renders.
  (Formlet.Render.List.Key -> Formlet.Render.List.Key) ->
  Forest options renders ~> Forest options renders
mapKey f =
  Formlet.Render.List.List
    <<< map (\r -> r { key = f r.key, render = mapKeyTree r.render })
    <<< un Formlet.Render.List.List
  where
  mapKeyTree :: Tree options renders ~> Tree options renders
  mapKeyTree = case _ of
    Leaf l -> Leaf l
    Node n -> Node n { children = mapKey f n.children }

-- | Embed a `Forest`-shaped Form into a `Tree`-shaped render representation as
-- | a node in the Tree while also setting the specified options. See `leaf`
-- | for a similar example.
node ::
  forall config options list record renders m value result.
  Prim.RowList.RowToList record list =>
  Formlet.Options.SetOptions list record options (Tree options renders) =>
  Record record ->
  Formlet.Form config (Forest options renders) m value result ->
  Formlet.Form config (Tree options renders) m value result
node record = Formlet.mapRender (Formlet.Options.set' record <<< Node <<< { options: Option.empty, children: _ })
