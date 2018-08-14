module Main where

import           Test.Hspec
import           Data.Monoid
import           HtmlToElm.Parser
import qualified Data.Text                     as T



main :: IO ()
main = hspec $ do
  describe "compare generated code with files" $ do
    it "does it" $ do
      expected <- mapM
        readFile
        [ "test/elm-sources/exampleSvg.elm"
        , "test/elm-sources/exampleHtml.elm"
        , "test/elm-sources/exampleBoth.elm"
        ]
      generated <- mapM
        (\f -> do
          source <- readFile f
          return $ (T.unpack $ parse $ T.pack source) <> "\n"
        )
        [ "test/html-sources/example-svg.html"
        , "test/html-sources/example-html.html"
        , "test/html-sources/example-both.html"
        ]
      generated `shouldBe` expected
