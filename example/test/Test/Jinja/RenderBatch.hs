module Test.Jinja.RenderBatch (spec) where

import Data.Aeson
import Data.Either (isLeft)
import Test.Hspec

import Jinja (renderJinjaBatch)

spec = do
  describe "renderJinjaBatch" $ do
    it "renders multiple templates with different contexts" $ do
      let template = "Hello {{ name }}!"
          contexts = [object ["name" .= ("Alice" :: String)],
                      object ["name" .= ("Bob" :: String)]]
      results <- renderJinjaBatch template contexts
      results `shouldBe` [Right "Hello Alice!", Right "Hello Bob!"]

    it "returns an error for an invalid template" $ do
      let template = "Hello {{ name }"
          contexts = [object ["name" .= ("Alice" :: String)],
                      object ["name" .= ("Bob" :: String)]]
      results <- renderJinjaBatch template contexts
      results `shouldSatisfy` all isLeft

    it "handles empty contexts gracefully" $ do
      let template = "Hello, stranger!"
          contexts = [object [], object []]
      results <- renderJinjaBatch template contexts
      results `shouldBe` [Right "Hello, stranger!", Right "Hello, stranger!"]

    it "handles error on faulty context" $ do
      let template = "Hello {{ item.name }}!"
          contexts = [object ["item" .= object ["name" .= ("Alice" :: String)]],
                      object ["name" .= ("Bob" :: String)]]
      results <- renderJinjaBatch template contexts
      results `shouldSatisfy` (\res -> length res == 2 && isLeft (res !! 1))
