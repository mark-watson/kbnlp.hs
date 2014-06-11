module Entities (humanNames, humanNames_s, countryNames) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sortBy, intersect, intersperse)

import Data.List (sortBy)

import Utils (splitWords, bigram, bigram_s, splitWordsKeepCase, trigram, trigram_s)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

import CountryNames (countryNamesOneWord, countryNamesTwoWords, countryNamesThreeWords)

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
  
countryNames1W wrds =
  filter (\w -> S.member w countryNamesOneWord) wrds
  
countryNames2W wrds =
  let twograms = bigram_s wrds in
  filter (\w -> S.member w countryNamesTwoWords) twograms
  
countryNames3W wrds =
  let threegrams = trigram_s wrds in
  filter (\w -> S.member w countryNamesThreeWords) threegrams

countryNames wrds =
  sortBy (\x y -> compare x y) $
    countryNames1W wrds ++ countryNames2W wrds ++ countryNames3W wrds
  
main = do
    let s = "The company is owned by John Smith, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends who grew up in Brazil, Buenos Aires, and the British Virgin Islands."
    print $ humanNames s
    print $ humanNames_s s
    print $ countryNames1W $ splitWordsKeepCase s
    print $ countryNames2W $ splitWordsKeepCase s
    print $ countryNames3W $ splitWordsKeepCase s
    print $ countryNames $ splitWordsKeepCase s
    


    
