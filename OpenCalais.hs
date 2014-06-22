module OpenCalais (results) where

import Network.HTTP

import qualified Data.Map as M
import qualified Data.Set as S

import Data.String.Utils (replace)
import Data.List (lines, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (maybe)

import System.Environment (getEnv)

calaisKey = getEnv "OPEN_CALAIS_KEY"

escape s = replace " " "%20" $ replace "\"" "&quot;" $ replace "'" "&#39" $ replace "<" "&lt;" $ replace ">" "&gt;" $ replace "/" "%2F" $ replace "&" "&amp;" s

baseParams = "<c:params xmlns:c=\"http://s.opencalais.com/1/pred/\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"><c:processingDirectives c:contentType=\"text/txt\" c:outputFormat=\"xml/rdf\"></c:processingDirectives><c:userDirectives c:allowDistribution=\"true\" c:allowSearch=\"true\" c:externalID=\"17cabs901\" c:submitter=\"ABC\"></c:userDirectives><c:externalMetadata></c:externalMetadata></c:params>"

results s = do
  key <- calaisKey
  let baseUrl = "http://api.opencalais.com/enlighten/calais.asmx/Enlighten?licenseID=" ++ key ++ "&content=" ++ (escape s) ++ "&paramsXML=" ++ (escape baseParams)
  ret <- simpleHTTP (getRequest baseUrl) >>= 
    fmap (take 10000) . getResponseBody 
  return $ map (\z -> splitOn ":" z) $
    filter (\x -> isInfixOf ": " x && length x < 40) (lines (replace "\r" "" ret))
  
main = do
  r <- results "Berlin Germany visited by George W. Bush to see IBM plant. Bush met with President Clinton."
  print $ r !! 0
  return r
  -- note: results are wrapped in a monad