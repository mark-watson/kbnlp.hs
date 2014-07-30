module Entities (companyNames, peopleNames, countryNames) where -- (humanNames, countryNames, companyNames) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sortBy, intersect, intersperse)
import Data.Set (empty)
import Data.List (sortBy)

import Utils (splitWords, bigram, bigram_s, splitWordsKeepCase, trigram, trigram_s, removeDuplicates)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

import PeopleDbPedia (peopleMap)

import CountryNamesDbpedia (countryMap)
import CountryNames (countryNamesOneWord, countryNamesTwoWords, countryNamesThreeWords)

import CompanyNamesDbpedia (companyMap)
import CompanyNames (companyNamesOneWord, companyNamesTwoWords, companyNamesThreeWords)

--  ...
 
xs `isSubsetOf` ys = all (`elem` ys) xs
    
  
helperNames1W wrds dbPediaMap wordMap =
  filter 
    (\x -> case (x) of
         (_, Just x) -> True
         _ -> False) $
    map (\w -> (w,
                let v = M.lookup w dbPediaMap in
                if v /= Nothing
                   then return (w, v)
                   else if (S.member w wordMap)
                           then Just (w, Just "")
                           else Nothing)) wrds    

helperNames2W wrds dbPediaMap wordMap =
  let twograms = bigram_s wrds in
  filter 
    (\x -> case (x) of
         (_, Just x) -> True
         _ -> False) $
    map (\w -> (w,
                let v = M.lookup w dbPediaMap in
                if v /= Nothing
                   then return (w, v)
                   else if (S.member w wordMap)
                           then Just (w, Just "")
                           else Nothing)) twograms    
    
helperNames3W wrds dbPediaMap wordMap =
  let threegrams = trigram_s wrds in
  filter 
    (\x -> case (x) of
         (_, Just x) -> True
         _ -> False) $
    map (\w -> (w,
                let v = M.lookup w dbPediaMap in
                if v /= Nothing
                   then return (w, v)
                   else if (S.member w wordMap)
                           then Just (w, Just "")
                           else Nothing)) threegrams    

companyNames1W wrds = helperNames1W wrds companyMap companyNamesOneWord
  
companyNames2W wrds = helperNames2W wrds companyMap companyNamesTwoWords

companyNames3W wrds = helperNames3W wrds companyMap companyNamesThreeWords

companyNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              companyNames1W wrds ++ companyNames2W wrds ++ companyNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns
  

countryNames1W wrds = helperNames1W wrds countryMap countryNamesOneWord
  
countryNames2W wrds = helperNames2W wrds countryMap countryNamesTwoWords

countryNames3W wrds = helperNames3W wrds countryMap countryNamesThreeWords

countryNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              countryNames1W wrds ++ countryNames2W wrds ++ countryNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


peopleNames1W wrds = helperNames1W wrds peopleMap Data.Set.empty
  
peopleNames2W wrds = helperNames2W wrds peopleMap Data.Set.empty

peopleNames3W wrds = helperNames3W wrds peopleMap Data.Set.empty

peopleNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              peopleNames1W wrds ++ peopleNames2W wrds ++ peopleNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


main = do
    let s = "As read in the San Francisco Chronicle, the company is owned by John Smith, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends who grew up in Brazil, Canada, Buenos Aires, and the British Virgin Islands. Apple Computer relased a new version of OS X yesterday. Brazil Brazil Brazil. John Smith bought stock in ConocoPhillips, Heinz, Hasbro, and General Motors"
    --print $ humanNames s
    print $ peopleNames $ splitWordsKeepCase s
    print $ countryNames $ splitWordsKeepCase s
    print $ companyNames $ splitWordsKeepCase s
    


    
