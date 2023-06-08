module Formlet.Render.List
  ( Key
  , List(..)
  , hoist
  , hoistWithKey
  , mapKey
  , singleton
  , toArray
  ) where

import CitizenNet.Prelude

import Formlet as Formlet

type Key =
  String

-- | A Form Render functor that represents a list-shaped Form, where each
-- | sub-form is uniquely identified by a `Key`.
-- |
-- | The unique `Key` is somewhat of an implementation detail, as, after all,
-- | its main use is to ensure that actually rendering the Form as a user
-- | interface doesn't produce any duplicate sub-forms.
--
-- NOTE: `List` does not have an instance of `HasOptions` and that's intended.
-- It means that we cannot, for example, validate a Form that is rendered as a
-- list, as we'd have to display a validation error message in all elements of
-- that list, which is not the behavior we want.
newtype List render (action :: Type) =
  List
    ( Array
        { key :: Key
        , render :: render action
        }
    )

derive instance newtypeList :: Newtype (List render action) _

derive newtype instance semigroupList :: Semigroup (List render action)

derive newtype instance monoidList :: Monoid (List render action)

derive instance functorList :: Functor render => Functor (List render)

-- | Change the inner `render` functor of the `List` render.
hoist ::
  forall render1 render2.
  render1 ~> render2 ->
  List render1 ~> List render2
hoist nt = List <<< map (\r -> r { render = nt r.render }) <<< un List

-- | Change the inner `render` functor of the `List` render while also having
-- | access to each element's unique identifier.
hoistWithKey ::
  forall render1 render2.
  (Key -> render1 ~> render2) ->
  List render1 ~> List render2
hoistWithKey nt = List <<< map (\r -> r { render = nt r.key r.render }) <<< un List

-- | Apply the given function to all keys in a `List`.
mapKey ::
  forall render.
  (Key -> Key) ->
  List render ~> List render
mapKey f = List <<< map (\r -> r { key = f r.key }) <<< un List

-- | Embed a singleton Form into a `List` render representation so that it may
-- | be combined with other forms.
singleton ::
  forall config render m value result.
  Key ->
  Formlet.Form config render m value result ->
  Formlet.Form config (List render) m value result
singleton key = Formlet.mapRender (List <<< pure <<< { key, render: _ })

-- | Convert a `List` render functor into an `Array`, discarding any `Key`
-- | information.
toArray ::
  forall render action.
  List render action ->
  Array (render action)
toArray = map _.render <<< un List
