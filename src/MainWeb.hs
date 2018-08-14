{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Web.Scotty
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text.Lazy                as LT
import           GHC.Generics
import           System.Environment
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as T

import           HtmlToElm.Parser

newtype Request = Request { bodyText :: T.Text } deriving (Show, Generic)

instance ToJSON Request
instance FromJSON Request

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  let port = read (fromMaybe "3300" mPort) :: Int
  scotty port $ do
    post "/" $ do
      b <- jsonData
      text $ LT.fromStrict $ parse (bodyText b)
