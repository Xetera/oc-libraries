{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( SearchResponse
  , parseSearch
  , Book
  ) where

import           Control.Applicative (ZipList)
import           Data.Aeson
import           Debug.Trace         (trace, traceM, traceShow, traceShowId,
                                      traceShowM)
import           GHC.Generics        hiding (css)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core   hiding (trace)

data Status = Status
  { library       :: String
  , shelfLocation :: String
  , callNo        :: String
  , status        :: String
  } deriving (Show, Generic)

data Book = Book
  { name        :: String
  , author      :: String
  , description :: Maybe String
  } deriving (Show, Generic)

--  , statuses :: [Status]
--  , available   :: Int
--  , holds       :: Int
--  , copies      :: Int
newtype SearchResponse =
  SearchResponse String

instance ToJSON Status

instance ToJSON Book

bookSelector :: String
bookSelector = ".results_cell"

authorSelector :: String
authorSelector = ".highlightMe.INITIAL_AUTHOR_SRCH"

descriptionSelector :: String
descriptionSelector = ".highlightMe.BIBSUMMARY"

titleSelector :: String
titleSelector = "a[id|=detailLink]"

tableSelector :: String
tableSelector = "table.sortable"

locationSelector :: String
locationSelector = "tbody tr"

locationCellSelector :: String
locationCellSelector = "td"

titles = css titleSelector ! "title"

authors = css authorSelector >>> deepest getText

description' = (css descriptionSelector >>> deepest getText >>> arr Just) `orElse` arr (const Nothing)

--statuses = css >>> tableSelector >>>

extractBooks doc = doc >>> css bookSelector

parse = readString [withParseHTML yes, withWarnings no]

parseSearch :: String -> IO [Book]
parseSearch html = do
  let books = extractBooks $ parse html
  runX $
    books >>>
    (titles &&& authors &&& description') >>> arr (\(name, (author, description)) -> Book {name, author, description})
