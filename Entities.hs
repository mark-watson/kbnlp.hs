module Entities (humanNames, humanNames_s) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sortBy, intersect, intersperse)

import Utils (splitWords, bigram, splitWordsKeepCase, trigram)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

filterTwoWordNames :: [[[Char]]] -> [[[Char]]]
filterTwoWordNames bigramS =
  filter (\b -> S.member (b !! 0) firstNames && S.member (b !! 1) lastNames) bigramS

filterThreeWordNames :: [[[Char]]] -> [[[Char]]]
filterThreeWordNames trigramS =
  filter (\b -> S.member (b !! 0) namePrefixes && S.member (b !! 1) firstNames && S.member (b !! 2) lastNames) trigramS
 
xs `isSubsetOf` ys = all (`elem` ys) xs
    
humanNames :: [Char] -> [[[Char]]]
humanNames s =
  let bigramS = filterTwoWordNames $ bigram $ splitWordsKeepCase s; 
      trigramS = filterThreeWordNames $ trigram $ splitWordsKeepCase s in
  trigramS ++ filter (\b -> (length (filter (b `isSubsetOf`) trigramS)) == 0)  bigramS

humanNames_s s =
  map (\l -> concat $ intersperse " " $ l) $ humanNames s
  
main = do
    let s = "The company is owned by John Smith, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends."
    print $ humanNames s
    print $ humanNames_s s
    


    
