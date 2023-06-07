module Test.Form2.Validation
  ( suite
  ) where

import CitizenNet.Prelude

import Data.Identity as Data.Identity
import Data.String as Data.String
import Form2 as Form2
import Form2.Options as Form2.Options
import Form2.Render as Form2.Render
import Form2.Validation as Form2.Validation
import Test.QuickCheck ((===))
import Test.Unit as Test.Unit
import Test.Unit.QuickCheck as Test.Unit.QuickCheck

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Form2.Validation" do
    Test.Unit.suite "Standard validators" do
      Test.Unit.test "`mustBe` should validate if a condition is satisfied" do
        Test.Unit.QuickCheck.quickCheck \condition error ->
          if condition then
            Right unit === Form2.Validation.runValidator (Form2.Validation.mustBe (const condition) { error }) unit
          else
            Left error === Form2.Validation.runValidator (Form2.Validation.mustBe (const condition) { error }) unit
      Test.Unit.test "`mustEqual` should validate whether two values are equal" do
        Test.Unit.QuickCheck.quickCheck \(a :: String) (b :: String) error ->
          if a == b then
            Right a === Form2.Validation.runValidator (Form2.Validation.mustEqual b { error }) a
          else
            Left error === Form2.Validation.runValidator (Form2.Validation.mustEqual b { error }) a
    Test.Unit.suite "`invalidate`" do
      Test.Unit.test "`invalidate` should invalidate a Form" do
        Test.Unit.QuickCheck.quickCheck \error ->
          Left [ error ] === Form2.validate (Form2.Validation.invalidate [ error ] identityForm) unit unit
      Test.Unit.test "`invalidate` should set the `errors` render option" do
        Test.Unit.QuickCheck.quickCheck \error ->
          Just [ error ] === Form2.Options.get (symbol { errors: _ }) (Form2.render (Form2.Validation.invalidate [ error ] identityForm) unit unit)
    Test.Unit.suite "`validated`" do
      Test.Unit.test "`validated` should validate the Form with the specified validator" do
        Test.Unit.QuickCheck.quickCheck \value ->
          let
            actual :: Either (Array String) String
            actual = Form2.validate validatedForm unit value

            expected :: Either (Array String) String
            expected = if Data.String.length value >= 6 then Left [ tooLongError ] else Right value
          in
            expected === actual
      Test.Unit.test "`validated` should render validation errors from the specified validator" do
        Test.Unit.QuickCheck.quickCheck \value ->
          let
            actual :: Maybe (Array String)
            actual = Form2.Options.get (symbol { errors: _ }) (Form2.render validatedForm unit value)

            expected :: Maybe (Array String)
            expected = if Data.String.length value >= 6 then Just [ tooLongError ] else Nothing
          in
            expected === actual
      Test.Unit.test "`validated` should not render validation errors from other `validated` applications in the same Form" do
        Test.Unit.QuickCheck.quickCheck \value error ->
          let
            form ::
              forall config.
              Form2.Form config (Form2.Render.Render _ (identity :: Data.Identity.Identity)) Data.Identity.Identity String String
            form = Form2.Validation.validated (isShorterThan 8 { error }) validatedForm

            actual :: Maybe (Array String)
            actual = Form2.Options.get (symbol { errors: _ }) (Form2.render form unit value)

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
            validator :: forall a. Form2.Validation.Validator a a
            validator = if required then Form2.Validation.Required pure else Form2.Validation.NotRequired pure

            form ::
              forall config options value.
              Form2.Form config (Form2.Render.Render (errors :: Array String, required :: Boolean | options) (identity :: Data.Identity.Identity)) Data.Identity.Identity value value
            form = Form2.Validation.validated validator identityForm

            actual :: Maybe Boolean
            actual = Form2.Options.get (symbol { required: _ }) (Form2.render form unit value)
          in
            Just required === actual
      Test.Unit.test "Applying `validated` twice on a Form should set the `required` render option based on how the specified `Validator`s compose" do
        Test.Unit.QuickCheck.quickCheck \(value :: String) required1 required2 ->
          let
            validator :: forall a. Boolean -> Form2.Validation.Validator a a
            validator = if _ then Form2.Validation.Required pure else Form2.Validation.NotRequired pure

            form ::
              forall config options value.
              Form2.Form config (Form2.Render.Render (errors :: Array String, required :: Boolean | options) (identity :: Data.Identity.Identity)) Data.Identity.Identity value value
            form =
              Form2.Validation.validated (validator required2)
                $ Form2.Validation.validated (validator required1)
                $ identityForm

            actual :: Maybe Boolean
            actual = Form2.Options.get (symbol { required: _ }) (Form2.render form unit value)
          in
            Just (required1 || required2) === actual

identityForm ::
  forall config options value.
  Form2.Form config (Form2.Render.Render options (identity :: Data.Identity.Identity)) Data.Identity.Identity value value
identityForm = Form2.form_ \_ _ -> Form2.Render.inj { identity: pure (pure identity) }

isShorterThan :: Int -> { error :: String } -> Form2.Validation.Validator String String
isShorterThan length { error } =
  Form2.Validation.NotRequired \str ->
    if Data.String.length str < length then Right str else Left error

tooLongError :: String
tooLongError = "Too long"

validatedForm ::
  forall config options.
  Form2.Form config (Form2.Render.Render (errors :: Array String, required :: Boolean | options) (identity :: Data.Identity.Identity)) Data.Identity.Identity String String
validatedForm = Form2.Validation.validated (isShorterThan 6 { error: tooLongError }) identityForm
