module Blanket.HTML.Web (
  parsePage
  ) where

import Network.HTTP

-- Takes a String URL (e.g. "http://www.google.com") and downloads and then parses it
parsePage :: String -> [Tag]
parsePage str = let rsp <- simpleHTTP (getRequest str)
                in 
  
  