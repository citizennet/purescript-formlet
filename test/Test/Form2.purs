module Test.Formlet
  ( main
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Const as Data.Const
import Data.Identity as Data.Identity
import Data.Newtype as Data.Newtype
import Formlet as Formlet
import Test.Formlet.Options as Test.Formlet.Options
import Test.Formlet.Render as Test.Formlet.Render
import Test.Formlet.Validation as Test.Formlet.Validation
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.Main as Test.Unit.Main
import Test.Unit.QuickCheck as Test.Unit.QuickCheck
import Test.Utils as Test.Utils

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    suite
    Test.Formlet.Options.suite
    Test.Formlet.Render.suite
    Test.Formlet.Validation.suite

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Formlet" do
    Test.Unit.suite "Form" do
      Test.Unit.test "`form_` makes the Form value available" do
        Test.Unit.QuickCheck.quickCheck \(value :: String) ->
          Data.Const.Const value === Formlet.render constForm unit value
      Test.Unit.test "`validate` should succeed in a Form without validation" do
        Test.Unit.QuickCheck.quickCheck \(a :: String) b ->
          Right (a <> b) === Formlet.validate applicativeForm unit { a, b }
      Test.Unit.test "`apply` should accumulate renders in a Semigroup" do
        Test.Unit.QuickCheck.quickCheck \(a :: String) b ->
          Data.Const.Const (a <> b) === Formlet.render applicativeForm unit { a, b }
      Test.Unit.test "`apply` should accumulate validation errors" do
        Test.Unit.QuickCheck.quickCheck \a b ->
          let
            expected :: Either (Array String) String
            expected = case a, b of
              Nothing, Nothing -> Left [ "Required", "Required" ]
              Nothing, Just _ -> Left [ "Required" ]
              Just _, Nothing -> Left [ "Required" ]
              Just a', Just b' -> Right (a' <> b')
          in
            expected === Formlet.validate applicativeFormRequired unit { a, b }
      Test.Unit.test "`alt` should accumulate renders in a Semigroup" do
        Test.Unit.QuickCheck.quickCheck \(a :: Array String) b ->
          Data.Const.Const (a <> b) === Formlet.render altForm unit { a, b }
      Test.Unit.test "`alt` should accumulate validation errors" do
        Test.Unit.QuickCheck.quickCheck \a b ->
          let
            expected :: Either (Array String) (Array String)
            expected = case a, b of
              [], [] -> Left [ "Required", "Required" ]
              [], b' -> Right b'
              a', [] -> Right a'
              a', _ -> Right a'
          in
            expected === Formlet.validate altForm unit { a, b }
      Test.Unit.test "`variant` should pick the correct form" do
        let
          form ::
            Formlet.Form Unit (Data.Const.Const String) Data.Identity.Identity (Variant (foo :: Maybe String, bar :: String)) String
          form =
            Formlet.variant
              { foo: Formlet.mapRender (Data.Newtype.over Data.Const.Const (fromMaybe "")) requiredForm
              , bar: constForm :: Formlet.Form Unit _ _ _ _
              }

          render :: Variant (foo :: Maybe String, bar :: String) -> String
          render = un Data.Const.Const <<< Formlet.render form unit
        Test.Utils.equal "foo" (render (variant { foo: Just "foo" }))
        Test.Utils.equal "" (render (variant { foo: Nothing }))
        Test.Utils.equal "bar" (render (variant { bar: "bar" }))
        Test.Utils.equal (Right "foo") (Formlet.validate form unit (variant { foo: Just "foo" }))
        Test.Utils.equal (Left [ "Required" ]) (Formlet.validate form unit (variant { foo: Nothing }))
        Test.Utils.equal (Right "bar") (Formlet.validate form unit (variant { bar: "bar" }))
      Test.Unit.test "`unvalidated` forms are always valid" do
        Test.Unit.QuickCheck.quickCheck \(a :: Maybe String) ->
          Right (Formlet.validate requiredForm unit a) === Formlet.validate (Formlet.unvalidated requiredForm) unit a
        Test.Unit.QuickCheck.quickCheck \(a :: Maybe String) b ->
          Right (Formlet.validate applicativeForm unit { a, b }) === Formlet.validate (Formlet.unvalidated applicativeForm) unit { a, b }
      Test.Unit.test "`invalid` is invalid" do
        Test.Unit.QuickCheck.quickCheck \(message :: String) ->
          let
            form :: forall config m value. Formlet.Form config (Data.Const.Const Unit) m value Unit
            form = Formlet.invalid [ message ]
          in
            Left [ message ] === Formlet.validate form unit unit
      Test.Unit.test "`listen` should capture a field's current value and allow updating it, returning any updates in sequence" do
        Test.Unit.QuickCheck.quickCheck \a b c ->
          let
            form :: forall config. Formlet.Form config Data.Identity.Identity Data.Identity.Identity (Array Int) (Array Int)
            form =
              Formlet.listen (\_ -> pure (Data.Array.cons c))
                $ Formlet.listen (\value -> pure $ if value == [ a ] then Data.Array.cons b else identity)
                $ Formlet.form_ \_ _ -> pure (pure (Data.Array.cons a))

            render :: Array Int -> Array Int -> Array Int
            render = un Data.Identity.Identity <<< un Data.Identity.Identity <<< Formlet.render form unit
          in
            [ c, b, a ] === render [] []
      Test.Unit.test "`listenResult` should capture a field's validated result and allow the value, returning any updates in sequence, but should not be called if the form becomes invalid" do
        Test.Unit.QuickCheck.quickCheck \a b c ->
          let
            form :: forall config. Formlet.Form config Data.Identity.Identity Data.Identity.Identity (Array Int) (Array Int)
            form =
              Formlet.listenResult (\_ -> pure (Data.Array.cons c))
                $ Formlet.listenResult (\_ -> pure (Data.Array.cons b))
                $ Formlet.form \_ ->
                    { render: \_ -> pure (pure (Data.Array.cons a))
                    , validate: \value -> if Data.Array.length value > 1 then Left "Invalid" else Right value
                    }

            render :: Array Int -> Array Int -> Array Int
            render = un Data.Identity.Identity <<< un Data.Identity.Identity <<< Formlet.render form unit
          in
            [ b, a ] === render [] []
    Test.Unit.suite "Wizard" do
      Test.Unit.test "Chaining Wizards show render only up to the last valid Form" do
        Test.Unit.QuickCheck.quickCheck \a b ->
          let
            expected :: Maybe String
            expected = case a, b of
              Just a', Nothing -> Just a'
              Just a', Just b' -> Just (a' <> b')
              Nothing, Nothing -> Nothing
              Nothing, Just _ -> Nothing
          in
            Data.Const.Const expected === Formlet.render wizardValidated unit { a, b }
      Test.Unit.test "`alt` should accumulate renders in a Semigroup" do
        Test.Unit.QuickCheck.quickCheck \(a :: Array String) b ->
          let
            expected :: Array String
            expected = case a, b of
              [], [] -> []
              [], b' -> b'
              a', [] -> a'
              a', _ -> a'
          in
            Data.Const.Const expected === Formlet.render wizardValidatedAlt unit { a, b }
      Test.Unit.test "`alt` should only fail if both cases are invalid" do
        Test.Unit.QuickCheck.quickCheck \a b ->
          let
            expected :: Either (Array String) (Array String)
            expected = case a, b of
              [], [] -> Left [ "Required" ]
              [], b' -> Right b'
              a', [] -> Right a'
              a', _ -> Right a'
          in
            expected === Formlet.validate wizardValidatedAlt unit { a, b }

altForm ::
  forall config value.
  Formlet.Form config (Data.Const.Const (Array value)) Data.Identity.Identity { a :: Array value, b :: Array value } (Array value)
altForm = Formlet.overRecord { a: _ } nonEmptyForm <|> Formlet.overRecord { b: _ } nonEmptyForm

applicativeForm ::
  forall config value.
  Monoid value =>
  Formlet.Form config (Data.Const.Const value) Data.Identity.Identity { a :: value, b :: value } value
applicativeForm = ado
  a <- Formlet.overRecord { a: _ } constForm
  b <- Formlet.overRecord { b: _ } constForm
  in a <> b

applicativeFormRequired ::
  forall config value.
  Monoid value =>
  Formlet.Form config (Data.Const.Const (Maybe value)) Data.Identity.Identity { a :: Maybe value, b :: Maybe value } value
applicativeFormRequired = ado
  a <- Formlet.overRecord { a: _ } requiredForm
  b <- Formlet.overRecord { b: _ } requiredForm
  in a <> b

constForm :: forall config m value. Formlet.Form config (Data.Const.Const value) m value value
constForm = Formlet.form_ \_ -> Data.Const.Const

nonEmptyForm :: forall config m value. Formlet.Form config (Data.Const.Const (Array value)) m (Array value) (Array value)
nonEmptyForm =
  Formlet.form \_ ->
    { render: Data.Const.Const
    , validate: \value -> if Data.Array.null value then Left "Required" else Right value
    }

requiredForm :: forall config m value. Formlet.Form config (Data.Const.Const (Maybe value)) m (Maybe value) value
requiredForm =
  Formlet.form \_ ->
    { render: Data.Const.Const
    , validate: note "Required"
    }

wizardValidated ::
  forall config value.
  Monoid value =>
  Formlet.Form config (Data.Const.Const (Maybe value)) Data.Identity.Identity { a :: (Maybe value), b :: (Maybe value) } value
wizardValidated =
  parallel do
    a <- sequential $ Formlet.overRecord { a: _ } requiredForm
    b <- sequential $ Formlet.overRecord { b: _ } requiredForm
    pure (a <> b)

wizardValidatedAlt ::
  forall config value.
  Formlet.Form config (Data.Const.Const (Array value)) Data.Identity.Identity { a :: Array value, b :: Array value } (Array value)
wizardValidatedAlt =
  parallel
    $ sequential (Formlet.overRecord { a: _ } nonEmptyForm)
    <|> sequential (Formlet.overRecord { b: _ } nonEmptyForm)
