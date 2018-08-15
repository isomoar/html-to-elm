{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Web.Scotty
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text.Lazy                as LT
import           GHC.Generics
import           Control.Monad.IO.Class         ( liftIO )
import           System.Environment
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as T
import           Data.Text.Lazy.Encoding       as TE
import           Data.ByteString.Lazy          as B
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           HtmlToElm.Parser

newtype Request = Request { bodyText :: T.Text } deriving (Show, Generic)

instance ToJSON Request
instance FromJSON Request

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  let port = read (fromMaybe "3300" mPort) :: Int
  scotty port $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "web")

    get "/" $ do
      h <- liftIO $ B.readFile "web/index.html"
      html $ TE.decodeUtf8 h

    post "/" $ do
      b <- jsonData
      json $ LT.fromStrict $ parse (bodyText b)
