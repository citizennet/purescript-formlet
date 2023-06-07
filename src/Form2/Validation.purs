module Form2.Validation
  ( Validator(..)
  , invalidate
  , isInt
  , isJust
  , isNonEmptyArray
  , isNonEmptySet
  , isNonEmptyString
  , isNumber
  , lift
  , maxStringLength
  , mustBe
  , mustEqual
  , optional
  , parse
  , relabel
  , required
  , runValidator
  , validated
  ) where

import CitizenNet.Prelude hiding (lift)

import Control.Alt as Control.Alt
import Data.Array.NonEmpty as Data.Array.NonEmpty
import Data.Int as Data.Int
import Data.Number as Data.Number
import Data.Profunctor as Data.Profunctor
import Data.Profunctor.Strong as Data.Profunctor.Strong
import Data.Set.NonEmpty as Data.Set.NonEmpty
import Data.String as Data.String
import Data.String.NonEmpty as Data.String.NonEmpty
import Data.Validation.Semigroup as Data.Validation.Semigroup
import Form2 as Form2
import Form2.Options as Form2.Options

-- | A `Validator` is essentially a function from the unvalidated type `a` to
-- | either an error message or the validated type `b`, plus a decoration that
-- | tells whether applying the validator to a Form should make the Form be
-- | considered 'required' or not.
data Validator a b
  = Required (a -> Either String b)
  | NotRequired (a -> Either String b)

derive instance Functor (Validator a)

instance Apply (Validator a) where
  apply = case _, _ of
    Required f, Required g -> Required (\a -> apply (f a) (g a))
    Required f, NotRequired g -> Required (\a -> apply (f a) (g a))
    NotRequired f, Required g -> Required (\a -> apply (f a) (g a))
    NotRequired f, NotRequired g -> NotRequired (\a -> apply (f a) (g a))

instance Applicative (Validator a) where
  pure = NotRequired <<< const <<< Right

instance Control.Alt.Alt (Validator a) where
  alt = case _, _ of
    Required f, Required g -> Required (\a -> f a <|> g a)
    Required f, NotRequired g -> NotRequired (\a -> f a <|> g a)
    NotRequired f, Required g -> NotRequired (\a -> f a <|> g a)
    NotRequired f, NotRequired g -> NotRequired (\a -> f a <|> g a)

instance Data.Profunctor.Profunctor Validator where
  dimap f g = case _ of
    Required v -> Required (map g <<< v <<< f)
    NotRequired v -> NotRequired (map g <<< v <<< f)

instance Data.Profunctor.Strong.Strong Validator where
  first = case _ of
    Required f -> Required $ injLeft f
    NotRequired f -> NotRequired $ injLeft f
    where
    injLeft f = case _ of
      Tuple a c -> (\b -> Tuple b c) <$> f a

  second = case _ of
    Required f -> Required $ injRight f
    NotRequired f -> NotRequired $ injRight f
    where
    injRight f = case _ of
      Tuple c a -> (Tuple c) <$> f a

instance Semigroupoid Validator where
  compose = case _, _ of
    Required f, Required g -> Required (f <=< g)
    Required f, NotRequired g -> Required (f <=< g)
    NotRequired f, Required g -> Required (f <=< g)
    NotRequired f, NotRequired g -> NotRequired (f <=< g)

instance Category Validator where
  identity = NotRequired pure

-- | Invalidate a Form, setting its validation error messages to the specified
-- | value and making the errros available in the render functor through the
-- | `errors :: Errors` render option.
invalidate ::
  forall config options render m value result.
  Form2.Options.HasOptions (errors :: Form2.Errors | options) render =>
  Functor m =>
  Form2.Errors ->
  Form2.Form config render m value result ->
  Form2.Form config render m value result
invalidate errors (Form2.Form f) =
  Form2.Form \config ->
    let
      { render } = f config
    in
      { render: Form2.Options.set (symbol { errors: _ }) errors <<< render
      , validate: \_ -> Data.Validation.Semigroup.invalid errors
      }

-- | A validator which verifies that its input can be parsed as an integer.
isInt :: { name :: String } -> Validator String Int
isInt { name } = parse (name <> " must be a whole number") Data.Int.fromString

-- | A validator which verifies that an optional field is specified.
isJust :: forall a. { name :: String } -> Validator (Maybe a) a
isJust { name } = required (parse (name <> " is required") identity)

-- | A validator which verifies that an input array is non-empty.
isNonEmptyArray :: forall a. { name :: String } -> Validator (Array a) (NonEmptyArray a)
isNonEmptyArray { name } = required (parse (name <> " cannot be empty") Data.Array.NonEmpty.fromArray)

-- | A validator which verifies that an input set is non-empty.
isNonEmptySet :: forall a. { name :: String } -> Validator (Set a) (Data.Set.NonEmpty.NonEmptySet a)
isNonEmptySet { name } = required (parse (name <> " cannot be empty") Data.Set.NonEmpty.fromSet)

-- | A validator which verifies that an input string is non-empty.
isNonEmptyString :: { name :: String } -> Validator String Data.String.NonEmpty.NonEmptyString
isNonEmptyString { name } = required (parse (name <> " is required") Data.String.NonEmpty.fromString)

-- | A validator which verifies that its input can be parsed as a number.
isNumber :: { name :: String } -> Validator String Number
isNumber { name } = parse (name <> " must be a number") Data.Number.fromString

-- | Lift a pure function into a `NotRequired` validator.
lift :: forall a b. (a -> b) -> Validator a b
lift f = map f identity

maxStringLength :: { name :: String } -> Int -> Validator String String
maxStringLength { name } maxLength = mustBe (\s -> Data.String.length s <= maxLength) { error: name <> " must be less than " <> show maxLength <> " characters" }

-- | A validator which verifies that its input fulfills a specified condition.
mustBe :: forall a. (a -> Boolean) -> { error :: String } -> Validator a a
mustBe cond { error } = NotRequired \value -> if cond value then pure value else Left error

-- | A validator which verifies that its input equals some value.
mustEqual :: forall a. Eq a => a -> { error :: String } -> Validator a a
mustEqual value1 = mustBe (_ == value1)

-- | Modify a validator to accept a less refined type with the given parser.
-- | Values that do not parse are mapped to the `Nothing` output, and any other
-- | valid input is mapped to `Just` with the result of the original validator.
optional :: forall a a' b. (a' -> Maybe a) -> Validator a b -> Validator a' (Maybe b)
optional f validator = NotRequired (traverse (runValidator validator) <<< f)

-- | Build a validator from an error message and a parsing function.
parse :: forall a b. String -> (a -> Maybe b) -> Validator a b
parse error f = NotRequired (note error <<< f)

-- | Turn a `NotRequired` validator into a `Required` one.
required :: forall a b. Validator a b -> Validator a b
required = case _ of
  NotRequired v -> Required v
  Required v -> Required v

-- | Change the error message of a validator.
relabel :: forall a b. String -> Validator a b -> Validator a b
relabel error = case _ of
  NotRequired v -> NotRequired (lmap (const error) <<< v)
  Required v -> Required (lmap (const error) <<< v)

-- | Run a `Validator`, turning it into a function from the unvalidated type `a`
-- | to either a `String` error or the validated type `b`.
runValidator :: forall a b. Validator a b -> a -> Either String b
runValidator = case _ of
  Required f -> f
  NotRequired f -> f

-- | Attach a validator to a Form, effectively modifying the validated result as
-- | well as making the errors available in the render functor through the
-- | `errors :: Errors` render option.
-- |
-- | This function also uses the "required-ness" information in the `Validator`
-- | to render the Form as 'required' or not through the `required :: Boolean`
-- | render option.
validated ::
  forall config options render m value a b.
  Form2.Options.HasOptions (errors :: Form2.Errors, required :: Boolean | options) render =>
  Functor m =>
  Validator a b ->
  Form2.Form config render m value a ->
  Form2.Form config render m value b
validated validator (Form2.Form f) =
  Form2.Form \config ->
    let
      { render, validate } = f config

      setRequired :: render ~> render
      setRequired render =
        let
          required' :: Boolean
          required' = case validator of
            Required _ -> true
            NotRequired _ -> do
              -- Applying a `NotRequired` `Validator` should not set the
              -- `required` option to `false`, unless it hasn't yet been set.
              fromMaybe false (Form2.Options.get (symbol { required: _ }) render)
        in
          Form2.Options.set (symbol { required: _ }) required' render
    in
      { render:
          \value ->
            let
              -- Here we ignore any validation errors that do not arise from the
              -- specified `validator` because we assume they've already been
              -- rendered in the inner form.
              mErrors :: Maybe Form2.Errors
              mErrors = case validate value of
                Data.Validation.Semigroup.V (Left _) -> Nothing
                Data.Validation.Semigroup.V (Right a) -> case runValidator validator a of
                  Left errors -> Just [ errors ]
                  Right _ -> Nothing
            in
              setRequired case mErrors of
                Nothing -> render value
                Just errors -> Form2.Options.set (symbol { errors: _ }) errors (render value)
      , validate:
          \value ->
            Data.Validation.Semigroup.V do
              a <- un Data.Validation.Semigroup.V (validate value)
              lmap pure (runValidator validator a)
      }
