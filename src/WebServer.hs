{-# LANGUAGE OverloadedStrings #-}

module WebServer
  ( startServer
  ) where

import           Control.Monad.IO.Class
import           Data.Text.Lazy         as T (pack)
import           Library                as L
import           Web.Scotty             as WS

startServer =
  scotty 3000 $
    get "/search" $ param "name" >>= liftIO . search >>= json
