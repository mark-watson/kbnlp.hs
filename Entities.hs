module Entities (entities) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sortBy, intersect)

import Categorize (splitWords, bigram)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

splitWordsKeepCase :: String -> [String]
splitWordsKeepCase = words . map (\c -> if elem c ".,;:!\n\t\"" then ' ' else c)

trigram :: [a] -> [[a]]
trigram [] = []
trigram [_] = []
trigram [_,_] = []
trigram xs = take 3 xs : trigram (tail xs)

filterTwoWordNames :: [[[Char]]] -> [[[Char]]]
filterTwoWordNames bigramS =
  filter (\b -> S.member (b !! 0) firstNames && S.member (b !! 1) lastNames) bigramS

filterThreeWordNames :: [[[Char]]] -> [[[Char]]]
filterThreeWordNames trigramS =
  filter (\b -> S.member (b !! 0) namePrefixes && S.member (b !! 1) firstNames && S.member (b !! 2) lastNames) trigramS
 
xs `isSubsetOf` ys = all (`elem` ys) xs
    
names :: [Char] -> [[[Char]]]
names s =
  let bigramS = filterTwoWordNames $ bigram $ splitWordsKeepCase s; 
      trigramS = filterThreeWordNames $ trigram $ splitWordsKeepCase s in
  trigramS ++ filter (\b -> (length (filter (b `isSubsetOf`) trigramS)) == 0)  bigramS

entities = ""
      
main = do
    let s = "The company is owned by John Smith and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends."
    let wrds = splitWordsKeepCase s
    print "Testing NER...."
    print $ S.member "Robert" firstNames
    print $ S.member "Watson" lastNames
    print $ S.member "Mrs" namePrefixes
    print $ S.member "XXXzzz" namePrefixes
    print wrds
    print $ trigram wrds
    --print $ trigram []
    print $ trigram ["foo"]
    print $ trigram ["foo", "bar"]
    print $ filterTwoWordNames $ bigram wrds
    print $ filterThreeWordNames $ trigram wrds
    print $ names s
    


    
