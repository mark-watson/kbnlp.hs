module Entities (humanNames, countryNames, companyNames) where

--import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sortBy, intersect, intersperse)

import Data.List (sortBy)

import Utils (splitWords, bigram, bigram_s, splitWordsKeepCase, trigram, trigram_s, removeDuplicates)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

import CountryNames (countryNamesOneWord, countryNamesTwoWords, countryNamesThreeWords)
import CompanyNames (companyNamesOneWord, companyNamesTwoWords, companyNamesThreeWords)

filterTwoWordNames :: [[[Char]]] -> [[[Char]]]
filterTwoWordNames bigramS =
  filter (\b -> S.member (b !! 0) firstNames && S.member (b !! 1) lastNames) bigramS

filterThreeWordNames :: [[[Char]]] -> [[[Char]]]
filterThreeWordNames trigramS =
  filter (\b -> S.member (b !! 0) namePrefixes && S.member (b !! 1) firstNames && S.member (b !! 2) lastNames) trigramS
 
xs `isSubsetOf` ys = all (`elem` ys) xs
    
humanNamesAsTokens :: [Char] -> [[[Char]]]
humanNamesAsTokens s =
  let bigramS = filterTwoWordNames $ bigram $ splitWordsKeepCase s; 
      trigramS = filterThreeWordNames $ trigram $ splitWordsKeepCase s in
  trigramS ++ filter (\b -> (length (filter (b `isSubsetOf`) trigramS)) == 0)  bigramS

humanNames s =
  removeDuplicates $ map (\l -> concat $ intersperse " " $ l) $ humanNamesAsTokens s
  
countryNames1W wrds =
  filter (\w -> S.member w countryNamesOneWord) wrds
  
countryNames2W wrds =
  let twograms = bigram_s wrds in
  filter (\w -> S.member w countryNamesTwoWords) twograms
  
countryNames3W wrds =
  let threegrams = trigram_s wrds in
  filter (\w -> S.member w countryNamesThreeWords) threegrams

countryNames wrds =
  removeDuplicates $ sortBy (\x y -> compare x y) $
    countryNames1W wrds ++ countryNames2W wrds ++ countryNames3W wrds
  
companyNames1W wrds =
  filter (\w -> S.member w companyNamesOneWord) wrds
  
companyNames2W wrds =
  let twograms = bigram_s wrds in
  filter (\w -> S.member w companyNamesTwoWords) twograms
  
companyNames3W wrds =
  let threegrams = trigram_s wrds in
  filter (\w -> S.member w companyNamesThreeWords) threegrams

companyNames wrds =
  removeDuplicates $ sortBy (\x y -> compare x y) $
    companyNames1W wrds ++ companyNames2W wrds ++ companyNames3W wrds
  
main = do
    let s = "As read in the San Francisco Chronicle, the company is owned by John Smith, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends who grew up in Brazil, Buenos Aires, and the British Virgin Islands. Apple Computer relased a new version of OS X yesterday. Brazil Brazil Brazil. John Smith John Smith."
    print $ humanNames s
    print $ countryNames $ splitWordsKeepCase s
    print $ companyNames $ splitWordsKeepCase s
    


    
