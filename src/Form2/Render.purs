module Form2.Render
  ( Render(..)
  , inj
  , match
  ) where

import CitizenNet.Prelude

import Data.Functor.Variant as Data.Functor.Variant
import Form2.Options as Form2.Options
import Option as Option
import Prim.Row as Prim.Row
import Prim.RowList as Prim.RowList
import Record as Record

-- | `Render` is a canonical Form render type that wraps other render types and
-- | adds a record of optional attributes `options` that grows monotonically
-- | when new information is added to any Form piece, like how transforming an
-- | input into a field adds `name :: String` and `description :: Maybe String`,
-- | or how validating a piece of Form adds `errors :: Errors`.
-- |
-- | This data type essentially encodes a "sum of product-functors", where each
-- | of the sum cases is given by the `renders` row, and all `options` are
-- | present in each of those cases as well.
-- |
-- | This Render functor is not a `Semigroup`, or a `Monoid`, which means that
-- | individual forms rendered with it cannot be combined with the `Applicative`
-- | instance. We understand the combination of renders is application-specific,
-- | and, as such, should be left up to the implementor.
newtype Render options (renders :: Row (Type -> Type)) (action :: Type) = Render
  { options :: Option options
  , render :: Data.Functor.Variant.VariantF renders action
  }

derive instance newtypeRender :: Newtype (Render options render action) _
derive instance functorRender :: Functor (Render options renders)

instance hasOptionsRender :: Form2.Options.HasOptions options (Render options renders) where
  get label (Render r) = Option.get label r.options
  set label value (Render r) = Render r { options = Option.set label value r.options }

-- | Inject a render functor into the specified label of the `Render` with an
-- | empty record of `options`.
inj ::
  forall label options record renders anything render action.
  Functor render =>
  IsSymbol label =>
  Prim.Row.Cons label (render action) () record =>
  Prim.Row.Cons label render anything renders =>
  Prim.RowList.RowToList record (Prim.RowList.Cons label (render action) Prim.RowList.Nil) =>
  Record record ->
  Render options renders action
inj record =
  Render
    { options: Option.empty
    , render: Data.Functor.Variant.inj (Proxy :: _ label) (Record.get (Proxy :: _ label) record)
    }

-- | Perform total pattern matching on all the possible render functors in a
-- | `Render`, handling all possible cases with a `Record`.
match ::
  forall list record options renders renders' action b.
  Prim.RowList.RowToList record list =>
  Data.Functor.Variant.VariantFMatchCases list renders' action b =>
  Prim.Row.Union renders' () renders =>
  Record record ->
  Render options renders action ->
  b
match record (Render { render }) = Data.Functor.Variant.match record render
