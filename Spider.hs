{-# LANGUAGE OverloadedStrings     #-}

-- experiments with web spidering

module Spider where                    

import Network.Curl.Download
import Text.HTML.TagSoup
import Data.Char

getContent p =
  case p of
    Right p -> p
    Left p  -> ""

main = do
  doc  <- openURI "http://haskell.org"
  tags <- openAsTags "http://haskell.org"
  --tags <- openAsXML "http://haskell.org"
  
  --let parsed = dropWhile (~/= "<h3>") tags
  return $ getContent doc