module Main (main) where

import Test.Hspec

import qualified Test.Jinja.RenderBatch as RenderBatch
import qualified Test.Jinja.RenderSingle as RenderSingle
import qualified Test.Jinja.RenderMultiple as RenderMultiple

main :: IO ()
main = do
  hspec $ do
    describe "Jinja Rendering Tests" $ do
      RenderSingle.spec
      RenderMultiple.spec
      RenderBatch.spec
