module Test.Form2.Render
  ( suite
  ) where

import CitizenNet.Prelude

import Form2.Options as Form2.Options
import Form2.Render as Form2.Render
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils

label :: Proxy "label"
label = Proxy

render :: forall options action. Form2.Render.Render options (foo :: Array) action
render = Form2.Render.inj { foo: [] }

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Form2.Render" do
    Test.Unit.suite "HasOptions instance" do
      Test.Unit.test "You get back what you set" do
        Test.Utils.equal (Just unit) (Form2.Options.get label (Form2.Options.set label unit render))
      Test.Unit.test "get >>> set >>> get is idempotent" do
        let
          render' :: forall options action. Form2.Render.Render (label :: Unit | options) (foo :: Array) action
          render' = Form2.Options.set label unit render

          expected :: Maybe Unit
          expected = Form2.Options.get label render'

          -- Ideally this test would read `map (flip (set label) render) (get label render) == Just render`
          -- instead, but `VariantF` does not have an `Eq` instance.
          actual :: Maybe Unit
          actual = Form2.Options.get label render' >>= flip (Form2.Options.set label) render' >>> Form2.Options.get label
        Test.Utils.equal expected actual
