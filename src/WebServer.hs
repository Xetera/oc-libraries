{-# LANGUAGE OverloadedStrings #-}

module WebServer
  ( startServer ) where

import Web.Scotty as WS
import Library as L
import           Data.Text.Lazy  as T (pack)
import Control.Monad.IO.Class

startServer =
  scotty 3000 $
    get "/search" $ param "name" >>= liftIO . search >>= json
