module Test.Jinja.RenderSingle (spec) where

import Data.Aeson
import Data.Either (isLeft)
import Data.Vector (fromList)
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

    it "handles filters: split" $ do
      let template = "Hello {{ names.split(',')[1] }}!"
          context = object ["names" .= ("Alice,Bob,Celine" :: String)]
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Hello Bob!"
      let template = "Hello {{ (names|split(',')|list)[1] }}!"
          context = object ["names" .= ("Alice,Bob,Celine" :: String)]
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Hello Bob!"

    it "handles filters: intercalate" $ do
      let template = "Names: {{ names|intercalate(',') }}"
          context = object ["names" .= fromList ["Alice", "Bob", "Celine" :: String]]
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Names: Alice,Bob,Celine"

    it "handles filters: ofAlphabet" $ do
      let template = "3: {{ 3|ofAlphabet }}, 5: {{ 5|ofAlphabet }}, 10: {{ 10|ofAlphabet }}, 30: {{ 30|ofAlphabet }}"
          context = object []
      result <- renderJinjaSingle template context
      result `shouldBe` Right "3: c, 5: e, 10: j, 30: ad"

    it "handles filters: roman" $ do
      let template = "3: {{ 3|roman }}, 543: {{ 543|roman }}, 4872: {{ 4872|roman }}"
          context = object []
      result <- renderJinjaSingle template context
      result `shouldBe` Right "3: III, 543: DXLIII, 4872: MMMMDCCCLXXII"
