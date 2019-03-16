{-# LANGUAGE OverloadedStrings #-}

module Library
  (search) where

import           Control.Monad.Catch        (MonadThrow)
import           Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Conduit       as C
import           Network.HTTP.Simple
import           System.Environment
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Monad.IO.Class
import Data.Text as T
import Parser

url :: String
url = "https://catalog.ocpl.org/client/en_US/default/search/results/"

searchRequest :: MonadThrow m => BS.ByteString -> m Request
searchRequest name = addSearch (Just name) . addUserAgent <$> parseRequest url
  where
    addUserAgent = setRequestHeader "User-Agent" ["memes"]
    addSearch item = C.setQueryString [("q", item)]

search :: String -> IO [String]
search name = do
  request <- searchRequest $ BS.pack name
  response <- httpLBS request
  let body = L8.unpack $ getResponseBody response
  runX $ parseSearch body

