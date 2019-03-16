{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( SearchResponse
  , parseSearch
  , Book
  ) where

import           Data.Aeson
import           GHC.Generics
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

data Status = Status
  { library       :: String
  , shelfLocation :: String
  , callNo        :: String
  , status        :: String
  } deriving (Show, Generic)

--data Book = Book
--  { name        :: String
--  , author      :: String
--  , description :: Maybe String
--  , statuses    :: [Status]
--  , available   :: Int
--  , holds       :: Int
--  , copies      :: Int
--  } deriving (Show, Generic)
data Book = Book
  { name   :: String
  , author :: String
  } deriving (Show, Generic)

newtype SearchResponse =
  SearchResponse String

instance ToJSON Book

bookSelector :: String
bookSelector = ".results_cell"

authorSelector :: String
authorSelector = ".highlightMe.INITIAL_AUTHOR_SRCH"

titleSelector :: String
titleSelector = "a[id|=detailLink]"

extractTitles doc = doc >>> css titleSelector ! "title"

extractBooks doc = doc >>> css bookSelector

extractAuthors doc = doc >>> css authorSelector >>> deepest getText

parse = readString [withParseHTML yes, withWarnings no]

parseSearch :: String -> IO [Book]
parseSearch html = do
  let books = extractBooks $ parse html
  authors <- runX $ extractAuthors books
  names <- runX $ extractTitles books
  return $ zipWith (\name author -> Book { name, author }) names authors
