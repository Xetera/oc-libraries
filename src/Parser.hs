module Parser
  ( SearchResponse, parseSearch
  ) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
newtype SearchResponse = SearchResponse String

titleSelector :: String
titleSelector = "a[id|=detailLink]"

extractTitles doc = doc >>> css titleSelector ! "title"

parse = readString [withParseHTML yes, withWarnings no]

parseSearch str = extractTitles $ parse str
