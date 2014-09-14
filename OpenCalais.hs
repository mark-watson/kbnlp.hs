-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

module OpenCalais (calaisResults) where

import Network.HTTP
import Network.HTTP.Base (urlEncode)

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Trans.Class (lift)

import Data.String.Utils (replace)
import Data.List (lines, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (maybe)

import System.Environment (getEnv)

calaisKey = getEnv "OPEN_CALAIS_KEY"

escape s = urlEncode s

baseParams = "<c:params xmlns:c=\"http://s.opencalais.com/1/pred/\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"><c:processingDirectives c:contentType=\"text/txt\" c:outputFormat=\"xml/rdf\"></c:processingDirectives><c:userDirectives c:allowDistribution=\"true\" c:allowSearch=\"true\" c:externalID=\"17cabs901\" c:submitter=\"ABC\"></c:userDirectives><c:externalMetadata></c:externalMetadata></c:params>"

calaisResults s = do
  key <- calaisKey
  let baseUrl = "http://api.opencalais.com/enlighten/calais.asmx/Enlighten?licenseID=" 
                ++ key ++ "&content=" ++ (escape s) ++ "&paramsXML=" 
                ++ (escape baseParams)
  ret <- simpleHTTP (getRequest baseUrl) >>= 
    fmap (take 10000) . getResponseBody 
  return $ map (\z -> splitOn ": " z) $
    filter (\x -> isInfixOf ": " x && length x < 40)
      (lines (replace "\r" "" ret))
  
main = do
  r <- calaisResults "Berlin Germany visited by George W. Bush to see IBM plant. Bush met with President Clinton. Bush said “felt it important to step it up”"
  print r
  return r
