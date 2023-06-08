module Test.Formlet.Validation
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Data.String as Data.String
import Formlet as Formlet
import Formlet.Options as Formlet.Options
import Formlet.Render as Formlet.Render
import Formlet.Validation as Formlet.Validation
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Formlet.Validation" do
    Test.Unit.suite "Standard validators" do
      Test.Unit.test "`mustBe` should validate if a condition is satisfied" do
        Test.Unit.QuickCheck.quickCheck \condition error ->
          if condition then
            Right unit === Formlet.Validation.runValidator (Formlet.Validation.mustBe (const condition) { error }) unit
          else
            Left error === Formlet.Validation.runValidator (Formlet.Validation.mustBe (const condition) { error }) unit
      Test.Unit.test "`mustEqual` should validate whether two values are equal" do
        Test.Unit.QuickCheck.quickCheck \(a :: String) (b :: String) error ->
          if a == b then
            Right a === Formlet.Validation.runValidator (Formlet.Validation.mustEqual b { error }) a
          else
            Left error === Formlet.Validation.runValidator (Formlet.Validation.mustEqual b { error }) a
    Test.Unit.suite "`invalidate`" do
      Test.Unit.test "`invalidate` should invalidate a Form" do
        Test.Unit.QuickCheck.quickCheck \error ->
          Left [ error ] === Formlet.validate (Formlet.Validation.invalidate [ error ] identityForm) unit unit
      Test.Unit.test "`invalidate` should set the `errors` render option" do
        Test.Unit.QuickCheck.quickCheck \error ->
          Just [ error ] === Formlet.Options.get (symbol { errors: _ }) (Formlet.render (Formlet.Validation.invalidate [ error ] identityForm) unit unit)
    Test.Unit.suite "`validated`" do
      Test.Unit.test "`validated` should validate the Form with the specified validator" do
        Test.Unit.QuickCheck.quickCheck \value ->
          let
            actual :: Either (Array String) String
            actual = Formlet.validate validatedForm unit value

            expected :: Either (Array String) String
            expected = if Data.String.length value >= 6 then Left [ tooLongError ] else Right value
          in
            expected === actual
      Test.Unit.test "`validated` should render validation errors from the specified validator" do
        Test.Unit.QuickCheck.quickCheck \value ->
          let
            actual :: Maybe (Array String)
            actual = Formlet.Options.get (symbol { errors: _ }) (Formlet.render validatedForm unit value)

            expected :: Maybe (Array String)
            expected = if Data.String.length value >= 6 then Just [ tooLongError ] else Nothing
          in
            expected === actual
      Test.Unit.test "`validated` should not render validation errors from other `validated` applications in the same Form" do
        Test.Unit.QuickCheck.quickCheck \value error ->
          let
            form ::
              forall config.
              Formlet.Form config (Formlet.Render.Render _ (identity :: Data.Identity.Identity)) Data.Identity.Identity String String
            form = Formlet.Validation.validated (isShorterThan 8 { error }) validatedForm

            actual :: Maybe (Array String)
            actual = Formlet.Options.get (symbol { errors: _ }) (Formlet.render form unit value)

            expected :: Maybe (Array String)
            expected =
              if Data.String.length value >= 6 then
                Just [ tooLongError ]
              else if Data.String.length value >= 8 then
                Just [ error ]
              else
                Nothing
          in
            expected === actual
      Test.Unit.test "`validated` should set the `required` render option based on the specified `Validator`" do
        Test.Unit.QuickCheck.quickCheck \(value :: String) required ->
          let
            validator :: forall a. Formlet.Validation.Validator a a
            validator = if required then Formlet.Validation.Required pure else Formlet.Validation.NotRequired pure

            form ::
              forall config options value.
              Formlet.Form config (Formlet.Render.Render (errors :: Array String, required :: Boolean | options) (identity :: Data.Identity.Identity)) Data.Identity.Identity value value
            form = Formlet.Validation.validated validator identityForm

            actual :: Maybe Boolean
            actual = Formlet.Options.get (symbol { required: _ }) (Formlet.render form unit value)
          in
            Just required === actual
      Test.Unit.test "Applying `validated` twice on a Form should set the `required` render option based on how the specified `Validator`s compose" do
        Test.Unit.QuickCheck.quickCheck \(value :: String) required1 required2 ->
          let
            validator :: forall a. Boolean -> Formlet.Validation.Validator a a
            validator = if _ then Formlet.Validation.Required pure else Formlet.Validation.NotRequired pure

            form ::
              forall config options value.
              Formlet.Form config (Formlet.Render.Render (errors :: Array String, required :: Boolean | options) (identity :: Data.Identity.Identity)) Data.Identity.Identity value value
            form =
              Formlet.Validation.validated (validator required2)
                $ Formlet.Validation.validated (validator required1)
                $ identityForm

            actual :: Maybe Boolean
            actual = Formlet.Options.get (symbol { required: _ }) (Formlet.render form unit value)
          in
            Just (required1 || required2) === actual

identityForm ::
  forall config options value.
  Formlet.Form config (Formlet.Render.Render options (identity :: Data.Identity.Identity)) Data.Identity.Identity value value
identityForm = Formlet.form_ \_ _ -> Formlet.Render.inj { identity: pure (pure identity) }

isShorterThan :: Int -> { error :: String } -> Formlet.Validation.Validator String String
isShorterThan length { error } =
  Formlet.Validation.NotRequired \str ->
    if Data.String.length str < length then Right str else Left error

tooLongError :: String
tooLongError = "Too long"

validatedForm ::
  forall config options.
  Formlet.Form config (Formlet.Render.Render (errors :: Array String, required :: Boolean | options) (identity :: Data.Identity.Identity)) Data.Identity.Identity String String
validatedForm = Formlet.Validation.validated (isShorterThan 6 { error: tooLongError }) identityForm
