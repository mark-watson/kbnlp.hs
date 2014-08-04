module Entities (companyNames, peopleNames, countryNames, cityNames, broadcastNetworkNames,
                 musicGroupNames, politicalPartyNames, tradeUnionNames, universityNames) where

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
import CityNamesDbpedia (cityMap)
 
import BroadcastNetworkNamesDbPedia (broadcastNetworkMap)
import MusicGroupNamesDbPedia (musicGroupMap)
import PoliticalPartyNamesDbPedia (politicalPartyMap)
import TradeUnionNamesDbPedia (tradeUnionMap)
import UniversityNamesDbPedia (universityMap)

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


cityNames1W wrds = helperNames1W wrds cityMap Data.Set.empty
  
cityNames2W wrds = helperNames2W wrds cityMap Data.Set.empty

cityNames3W wrds = helperNames3W wrds cityMap Data.Set.empty

cityNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              cityNames1W wrds ++ cityNames2W wrds ++ cityNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns



broadcastNetworkNames1W wrds = helperNames1W wrds broadcastNetworkMap Data.Set.empty
  
broadcastNetworkNames2W wrds = helperNames2W wrds broadcastNetworkMap Data.Set.empty

broadcastNetworkNames3W wrds = helperNames3W wrds broadcastNetworkMap Data.Set.empty

broadcastNetworkNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              broadcastNetworkNames1W wrds ++ broadcastNetworkNames2W wrds ++ broadcastNetworkNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


musicGroupNames1W wrds = helperNames1W wrds musicGroupMap Data.Set.empty
  
musicGroupNames2W wrds = helperNames2W wrds musicGroupMap Data.Set.empty

musicGroupNames3W wrds = helperNames3W wrds musicGroupMap Data.Set.empty

musicGroupNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              musicGroupNames1W wrds ++ musicGroupNames2W wrds ++ musicGroupNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


politicalPartyNames1W wrds = helperNames1W wrds politicalPartyMap Data.Set.empty
  
politicalPartyNames2W wrds = helperNames2W wrds politicalPartyMap Data.Set.empty

politicalPartyNames3W wrds = helperNames3W wrds politicalPartyMap Data.Set.empty

politicalPartyNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              politicalPartyNames1W wrds ++ politicalPartyNames2W wrds ++ politicalPartyNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


tradeUnionNames1W wrds = helperNames1W wrds tradeUnionMap Data.Set.empty
  
tradeUnionNames2W wrds = helperNames2W wrds tradeUnionMap Data.Set.empty

tradeUnionNames3W wrds = helperNames3W wrds tradeUnionMap Data.Set.empty

tradeUnionNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              tradeUnionNames1W wrds ++ tradeUnionNames2W wrds ++ tradeUnionNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

universityNames1W wrds = helperNames1W wrds universityMap Data.Set.empty
  
universityNames2W wrds = helperNames2W wrds universityMap Data.Set.empty

universityNames3W wrds = helperNames3W wrds universityMap Data.Set.empty

universityNames wrds =
  let cns = removeDuplicates $ sortBy (\x y -> compare x y) $
              universityNames1W wrds ++ universityNames2W wrds ++ universityNames3W wrds in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


main = do
    let s = "As read in the San Francisco Chronicle, the company is owned by John Smith, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends who grew up in Brazil, Canada, Buenos Aires, and the British Virgin Islands. Apple Computer relased a new version of OS X yesterday. Brazil Brazil Brazil. John Smith bought stock in ConocoPhillips, Heinz, Hasbro, and General Motors, Fox Sports Radio. I listen to B J Cole. Awami National Party is a political party. ALAEA is a trade union. She went to Brandeis University."
    --print $ humanNames s
    print $ peopleNames $ splitWordsKeepCase s
    print $ countryNames $ splitWordsKeepCase s
    print $ companyNames $ splitWordsKeepCase s
    print $ cityNames $ splitWordsKeepCase s
    print $ broadcastNetworkNames $ splitWordsKeepCase s
    print $ musicGroupNames $ splitWordsKeepCase s
    print $ politicalPartyNames $ splitWordsKeepCase s
    print $ tradeUnionNames $ splitWordsKeepCase s
    print $ universityNames $ splitWordsKeepCase s
    


    
