module Test.Form2
  ( main
  ) where

import CitizenNet.Prelude

import Data.Array as Data.Array
import Data.Const as Data.Const
import Data.Identity as Data.Identity
import Data.Newtype as Data.Newtype
import Form2 as Form2
import Test.Form2.Options as Test.Form2.Options
import Test.Form2.Render as Test.Form2.Render
import Test.Form2.Validation as Test.Form2.Validation
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.Main as Test.Unit.Main
import Test.Unit.QuickCheck as Test.Unit.QuickCheck
import Test.Utils as Test.Utils

main :: Effect Unit
main =
  Test.Unit.Main.runTest do
    suite
    Test.Form2.Options.suite
    Test.Form2.Render.suite
    Test.Form2.Validation.suite

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Form2" do
    Test.Unit.suite "Form" do
      Test.Unit.test "`form_` makes the Form value available" do
        Test.Unit.QuickCheck.quickCheck \(value :: String) ->
          Data.Const.Const value === Form2.render constForm unit value
      Test.Unit.test "`validate` should succeed in a Form without validation" do
        Test.Unit.QuickCheck.quickCheck \(a :: String) b ->
          Right (a <> b) === Form2.validate applicativeForm unit { a, b }
      Test.Unit.test "`apply` should accumulate renders in a Semigroup" do
        Test.Unit.QuickCheck.quickCheck \(a :: String) b ->
          Data.Const.Const (a <> b) === Form2.render applicativeForm unit { a, b }
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
            expected === Form2.validate applicativeFormRequired unit { a, b }
      Test.Unit.test "`alt` should accumulate renders in a Semigroup" do
        Test.Unit.QuickCheck.quickCheck \(a :: Array String) b ->
          Data.Const.Const (a <> b) === Form2.render altForm unit { a, b }
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
            expected === Form2.validate altForm unit { a, b }
      Test.Unit.test "`variant` should pick the correct form" do
        let
          form ::
            Form2.Form Unit (Data.Const.Const String) Data.Identity.Identity (Variant (foo :: Maybe String, bar :: String)) String
          form =
            Form2.variant
              { foo: Form2.mapRender (Data.Newtype.over Data.Const.Const (fromMaybe "")) requiredForm
              , bar: constForm :: Form2.Form Unit _ _ _ _
              }

          render :: Variant (foo :: Maybe String, bar :: String) -> String
          render = un Data.Const.Const <<< Form2.render form unit
        Test.Utils.equal "foo" (render (variant { foo: Just "foo" }))
        Test.Utils.equal "" (render (variant { foo: Nothing }))
        Test.Utils.equal "bar" (render (variant { bar: "bar" }))
        Test.Utils.equal (Right "foo") (Form2.validate form unit (variant { foo: Just "foo" }))
        Test.Utils.equal (Left [ "Required" ]) (Form2.validate form unit (variant { foo: Nothing }))
        Test.Utils.equal (Right "bar") (Form2.validate form unit (variant { bar: "bar" }))
      Test.Unit.test "`unvalidated` forms are always valid" do
        Test.Unit.QuickCheck.quickCheck \(a :: Maybe String) ->
          Right (Form2.validate requiredForm unit a) === Form2.validate (Form2.unvalidated requiredForm) unit a
        Test.Unit.QuickCheck.quickCheck \(a :: Maybe String) b ->
          Right (Form2.validate applicativeForm unit { a, b }) === Form2.validate (Form2.unvalidated applicativeForm) unit { a, b }
      Test.Unit.test "`invalid` is invalid" do
        Test.Unit.QuickCheck.quickCheck \(message :: String) ->
          let
            form :: forall config m value. Form2.Form config (Data.Const.Const Unit) m value Unit
            form = Form2.invalid [ message ]
          in
            Left [ message ] === Form2.validate form unit unit
      Test.Unit.test "`listen` should capture a field's current value and allow updating it, returning any updates in sequence" do
        Test.Unit.QuickCheck.quickCheck \a b c ->
          let
            form :: forall config. Form2.Form config Data.Identity.Identity Data.Identity.Identity (Array Int) (Array Int)
            form =
              Form2.listen (\_ -> pure (Data.Array.cons c))
                $ Form2.listen (\value -> pure $ if value == [ a ] then Data.Array.cons b else identity)
                $ Form2.form_ \_ _ -> pure (pure (Data.Array.cons a))

            render :: Array Int -> Array Int -> Array Int
            render = un Data.Identity.Identity <<< un Data.Identity.Identity <<< Form2.render form unit
          in
            [ c, b, a ] === render [] []
      Test.Unit.test "`listenResult` should capture a field's validated result and allow the value, returning any updates in sequence, but should not be called if the form becomes invalid" do
        Test.Unit.QuickCheck.quickCheck \a b c ->
          let
            form :: forall config. Form2.Form config Data.Identity.Identity Data.Identity.Identity (Array Int) (Array Int)
            form =
              Form2.listenResult (\_ -> pure (Data.Array.cons c))
                $ Form2.listenResult (\_ -> pure (Data.Array.cons b))
                $ Form2.form \_ ->
                    { render: \_ -> pure (pure (Data.Array.cons a))
                    , validate: \value -> if Data.Array.length value > 1 then Left "Invalid" else Right value
                    }

            render :: Array Int -> Array Int -> Array Int
            render = un Data.Identity.Identity <<< un Data.Identity.Identity <<< Form2.render form unit
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
            Data.Const.Const expected === Form2.render wizardValidated unit { a, b }
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
            Data.Const.Const expected === Form2.render wizardValidatedAlt unit { a, b }
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
            expected === Form2.validate wizardValidatedAlt unit { a, b }

altForm ::
  forall config value.
  Form2.Form config (Data.Const.Const (Array value)) Data.Identity.Identity { a :: Array value, b :: Array value } (Array value)
altForm = Form2.overRecord { a: _ } nonEmptyForm <|> Form2.overRecord { b: _ } nonEmptyForm

applicativeForm ::
  forall config value.
  Monoid value =>
  Form2.Form config (Data.Const.Const value) Data.Identity.Identity { a :: value, b :: value } value
applicativeForm = ado
  a <- Form2.overRecord { a: _ } constForm
  b <- Form2.overRecord { b: _ } constForm
  in a <> b

applicativeFormRequired ::
  forall config value.
  Monoid value =>
  Form2.Form config (Data.Const.Const (Maybe value)) Data.Identity.Identity { a :: Maybe value, b :: Maybe value } value
applicativeFormRequired = ado
  a <- Form2.overRecord { a: _ } requiredForm
  b <- Form2.overRecord { b: _ } requiredForm
  in a <> b

constForm :: forall config m value. Form2.Form config (Data.Const.Const value) m value value
constForm = Form2.form_ \_ -> Data.Const.Const

nonEmptyForm :: forall config m value. Form2.Form config (Data.Const.Const (Array value)) m (Array value) (Array value)
nonEmptyForm =
  Form2.form \_ ->
    { render: Data.Const.Const
    , validate: \value -> if Data.Array.null value then Left "Required" else Right value
    }

requiredForm :: forall config m value. Form2.Form config (Data.Const.Const (Maybe value)) m (Maybe value) value
requiredForm =
  Form2.form \_ ->
    { render: Data.Const.Const
    , validate: note "Required"
    }

wizardValidated ::
  forall config value.
  Monoid value =>
  Form2.Form config (Data.Const.Const (Maybe value)) Data.Identity.Identity { a :: (Maybe value), b :: (Maybe value) } value
wizardValidated =
  parallel do
    a <- sequential $ Form2.overRecord { a: _ } requiredForm
    b <- sequential $ Form2.overRecord { b: _ } requiredForm
    pure (a <> b)

wizardValidatedAlt ::
  forall config value.
  Form2.Form config (Data.Const.Const (Array value)) Data.Identity.Identity { a :: Array value, b :: Array value } (Array value)
wizardValidatedAlt =
  parallel
    $ sequential (Form2.overRecord { a: _ } nonEmptyForm)
    <|> sequential (Form2.overRecord { b: _ } nonEmptyForm)
