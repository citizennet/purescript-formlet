module Formlet.Initial
  ( class Initial
  , class InitialRecord
  , initial
  , initialRecordBuilder
  ) where

import CitizenNet.Prelude

import Data.Map as Data.Map
import Data.Set as Data.Set
import Prim.Row as Prim.Row
import Prim.RowList as Prim.RowList
import Record.Builder as Record.Builder

-- | Provides values for primitive data types to be used as *initial* or *empty*
-- | values in a `Form`.
class Initial a where
  initial :: a

-- | Helper class used to define the `Initial` instance for `Record`.
class InitialRecord (rl :: Prim.RowList.RowList Type) r' r | rl -> r' r where
  initialRecordBuilder :: Proxy rl -> Record.Builder.Builder { | r' } { | r }

instance initialRecordNil :: InitialRecord Prim.RowList.Nil () () where
  initialRecordBuilder _ = identity

instance initialRecordCons ::
  ( IsSymbol l
  , Initial a
  , InitialRecord tail r' tailR
  , Prim.Row.Lacks l tailR
  , Prim.Row.Cons l a tailR r
  ) =>
  InitialRecord (Prim.RowList.Cons l a tail) r' r where
  initialRecordBuilder _ =
    Record.Builder.insert (Proxy :: Proxy l) (initial :: a)
      <<< initialRecordBuilder (Proxy :: Proxy tail)

------------
-- Instances
------------
instance initialArray :: Initial (Array a) where
  initial = []

instance initialBoolean :: Initial Boolean where
  initial = false

instance initialEither :: Initial a => Initial (Either a b) where
  initial = Left initial

instance initialInt :: Initial Int where
  initial = 0

instance initialMap :: Initial (Map k a) where
  initial = Data.Map.empty

instance initialMaybe :: Initial (Maybe a) where
  initial = Nothing

instance initialNonEmptyArray :: Initial a => Initial (NonEmptyArray a) where
  initial = pure initial

instance initialNumber :: Initial Number where
  initial = 0.0

instance initialRecord :: (Prim.RowList.RowToList r rl, InitialRecord rl () r) => Initial (Record r) where
  initial = Record.Builder.build (initialRecordBuilder (Proxy :: Proxy rl)) {}

instance initialSet :: Initial (Set a) where
  initial = Data.Set.empty

instance initialString :: Initial String where
  initial = ""

instance initialUnit :: Initial Unit where
  initial = unit
