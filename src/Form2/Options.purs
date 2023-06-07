module Form2.Options
  ( class HasOptions
  , class SetOptions
  , get
  , set
  , modify
  , set'
  , set''
  ) where

import CitizenNet.Prelude

import Prim.Row as Prim.Row
import Prim.RowList as Prim.RowList
import Record as Record

-- | `HasOptions` represents Render functors that support labeled options
-- | that can be read and modified. This is useful to model optional render
-- | attributes that grow when new pieces of information are added to a Form.
-- |
-- | One example is that, before validation, a Form's render does not have the
-- | `errors :: Maybe (Array String)` option (see `Form2.Validation` for more
-- | information on this concrete case).
class
  Functor render <=
  HasOptions options render
  | render -> options where
  -- | Attempt to read a labeled option from a Render functor. If it exists, the
  -- | option's value is wrapped in a `Just`, otherwise we return `Nothing`.
  get ::
    forall proxy label value options' a.
    IsSymbol label =>
    Prim.Row.Cons label value options' options =>
    proxy label ->
    render a ->
    Maybe value
  -- | Set an option in a Render functor if the label/key exists in the
  -- | `options` row.
  set ::
    forall proxy label value options' a.
    IsSymbol label =>
    Prim.Row.Cons label value options' options =>
    proxy label ->
    value ->
    render a ->
    render a

-- | Helper class used to define `set'`. This iterates a `RowList` setting options
-- | in the specified `render` functor.
class
  HasOptions options render <=
  SetOptions (list :: Prim.RowList.RowList Type) (record :: Row Type) (options :: Row Type) render
  | list -> record
  , render -> options where
  set'' :: forall proxy a. proxy list -> Record record -> render a -> render a

instance setOptionsNil :: HasOptions options render => SetOptions Prim.RowList.Nil record options render where
  set'' _ _ = identity
else instance setOptionsCons ::
  ( HasOptions options render
  , IsSymbol label
  , Prim.Row.Cons label value record' record
  , Prim.Row.Cons label value options' options
  , SetOptions list record options render
  ) =>
  SetOptions (Prim.RowList.Cons label value list) record options render where
  set'' _ record =
    set (Proxy :: _ label) (Record.get (Proxy :: _ label) record)
      <<< set'' (Proxy :: _ list) record

-- | Attempt to modify an option identified by the `label` key in a Render
-- | functor that has options. If it exists, the given function is applied to
-- | the value, otherwise there is no change to the set of options.
modify ::
  forall proxy label value options' options render a.
  HasOptions options render =>
  IsSymbol label =>
  Prim.Row.Cons label value options' options =>
  proxy label ->
  (value -> value) ->
  render a ->
  render a
modify label f render = case get label render of
  Nothing -> render
  Just a -> set label (f a) render

-- | Set multiple options in a Render functor at once if all labels/keys from
-- | the `record` row exist in the `options` row.
set' ::
  forall list record options render a.
  Prim.RowList.RowToList record list =>
  SetOptions list record options render =>
  Record record ->
  render a ->
  render a
set' = set'' (Proxy :: _ list)
