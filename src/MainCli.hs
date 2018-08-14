module Main where

import HtmlToElm.Parser
import qualified Data.Text as T

main :: IO ()
main = do
  str <- getContents
  putStrLn (T.unpack $ parse $ T.pack str)
