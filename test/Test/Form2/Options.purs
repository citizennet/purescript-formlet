module Test.Form2.Options
  ( suite
  ) where

import CitizenNet.Prelude

import Form2.Options as Form2.Options
import Form2.Render as Form2.Render
import Test.Unit as Test.Unit
import Test.Utils as Test.Utils

render :: forall options action. Form2.Render.Render options (foo :: Array) action
render = Form2.Render.inj { foo: [] }

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Form2.Options" do
    Test.Unit.test "`set'` should set all the specified options" do
      let
        render' ::
          forall options action.
          Form2.Render.Render (foo :: String, bar :: String, baz :: String | options) (foo :: Array) action
        render' = Form2.Options.set' { foo: "foo", bar: "bar", baz: "baz" } render
      Test.Utils.equal (Just "foo") (Form2.Options.get (symbol { foo: _ }) render')
      Test.Utils.equal (Just "bar") (Form2.Options.get (symbol { bar: _ }) render')
      Test.Utils.equal (Just "baz") (Form2.Options.get (symbol { baz: _ }) render')
