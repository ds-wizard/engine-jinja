module Test.Jinja.RenderSingle (spec) where

import Data.Aeson
import Data.Either (isLeft)
import Test.Hspec

import Jinja (renderJinjaSingle)

spec = do
  describe "renderJinjaSingle" $ do

    it "renders a simple template with a single context" $ do
      let template = "Hello {{ name }}!"
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Hello Alice!"

    it "returns an error for an invalid template" $ do
      let template = "Hello {{ name }"
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaSingle template context
      result `shouldBe` Left "Error preparing Jinja template: unexpected '}'."

    it "handles empty context gracefully" $ do
      let template = "Hello, stranger!"
          context = object []
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Hello, stranger!"
