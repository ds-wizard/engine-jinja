module Test.Jinja.RenderMultiple (spec) where

import Data.Aeson
import Data.Either (isLeft)
import Data.Vector (fromList)
import Test.Hspec

import Jinja (renderJinjaMultiple)

spec = do
  describe "renderJinjaMultiple" $ do

    it "renders two templates with a single context" $ do
      let templates = ["Hello {{ name }}!", "Goodbye {{ name }}!"]
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Right "Hello Alice!", Right "Goodbye Alice!"]

    it "returns an error for an invalid template" $ do
      let templates = ["Hello {{ name }", "Goodbye {{ name }}!"]
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Left "Error preparing Jinja template: unexpected '}'.", Right "Goodbye Alice!"]

    it "handles empty context gracefully" $ do
      let templates = ["Hello, stranger!"]
          context = object []
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Right "Hello, stranger!"]

    it "handles disjoint variables in multiple templates" $ do
      let templates = ["Hello {{ name }}!", "Your age is {{ age }}."]
          context = object ["name" .= ("Alice" :: String), "age" .= (30 :: Int)]
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Right "Hello Alice!", Right "Your age is 30."]
