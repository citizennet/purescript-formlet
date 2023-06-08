module Test.Formlet.Render
  ( suite
  ) where

import CitizenNet.Prelude

import Formlet.Options as Formlet.Options
import Formlet.Render as Formlet.Render
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils

label :: Proxy "label"
label = Proxy

render :: forall options action. Formlet.Render.Render options (foo :: Array) action
render = Formlet.Render.inj { foo: [] }

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Formlet.Render" do
    Test.Unit.suite "HasOptions instance" do
      Test.Unit.test "You get back what you set" do
        Test.Utils.equal (Just unit) (Formlet.Options.get label (Formlet.Options.set label unit render))
      Test.Unit.test "get >>> set >>> get is idempotent" do
        let
          render' :: forall options action. Formlet.Render.Render (label :: Unit | options) (foo :: Array) action
          render' = Formlet.Options.set label unit render

          expected :: Maybe Unit
          expected = Formlet.Options.get label render'

          -- Ideally this test would read `map (flip (set label) render) (get label render) == Just render`
          -- instead, but `VariantF` does not have an `Eq` instance.
          actual :: Maybe Unit
          actual = Formlet.Options.get label render' >>= flip (Formlet.Options.set label) render' >>> Formlet.Options.get label
        Test.Utils.equal expected actual
