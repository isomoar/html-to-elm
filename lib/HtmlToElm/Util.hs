{-# LANGUAGE OverloadedStrings #-}

module HtmlToElm.Util
  ( toCamel
  , wrap
  , parseStyle
  , parseInt
  )
where

import           Data.List
import           Data.List.Split                ( splitOneOf )
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Text.Casing                   as C
import           Text.Read

toCamel :: T.Text -> T.Text
toCamel =
  T.pack
    . C.toCamel
    . C.fromWords
    . intercalate " "
    . splitOneOf "-:"
    . T.unpack

wrap :: T.Text -> T.Text
wrap = T.pack . show . T.strip

parseStyle :: T.Text -> T.Text
parseStyle s = "[" <> list s <> "]"
 where
  list = T.intercalate ", " . fmap pair . filter ((/=) "") . T.splitOn ";"
  pair =
    (\(a, b) -> "(" <> wrap a <> ", " <> wrap (T.tail b) <> ")") . T.breakOn ":"

parseInt :: T.Text -> T.Text
parseInt = T.pack . show . fromMaybe "0" . readMaybe . T.unpack
