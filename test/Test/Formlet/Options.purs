module Test.Formlet.Options
  ( suite
  ) where

import CitizenNet.Prelude

import Formlet.Options as Formlet.Options
import Formlet.Render as Formlet.Render
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

render :: forall options action. Formlet.Render.Render options (foo :: Array) action
render = Formlet.Render.inj { foo: [] }

suite :: Test.Unit.TestSuite
suite =
  Test.Unit.suite "Test.Formlet.Options" do
    Test.Unit.test "`set'` should set all the specified options" do
      let
        render' ::
          forall options action.
          Formlet.Render.Render (foo :: String, bar :: String, baz :: String | options) (foo :: Array) action
        render' = Formlet.Options.set' { foo: "foo", bar: "bar", baz: "baz" } render
      Test.Unit.Assert.equal (Just "foo") (Formlet.Options.get (symbol { foo: _ }) render')
      Test.Unit.Assert.equal (Just "bar") (Formlet.Options.get (symbol { bar: _ }) render')
      Test.Unit.Assert.equal (Just "baz") (Formlet.Options.get (symbol { baz: _ }) render')
