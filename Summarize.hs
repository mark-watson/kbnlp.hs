module Summarize (summarize) where

import qualified Data.Map as M

import Categorize (bestCategories)
import Sentence (segment)
import Utils (splitWords, bigram_s)

import Category1Gram (onegrams)
import Category2Gram (twograms)

scoreSentenceHelper words scoreMap = -- just use 1grams for now
  foldl (+) 0 $ map (\word ->  M.findWithDefault 0.0 word scoreMap) words

safeLookup key alist =
  let x = lookup key alist in
  case x of
    Just v -> v
    Nothing -> 0
 
scoreSentenceHelper2 words catDataMaps bestCategories =
  map (\(category, aMap) -> 
        (category, (safeLookup category bestCategories) * 
                   (scoreSentenceHelper words aMap))) catDataMaps

scoreSentenceHelper3 words catDataMaps bestCategories =  
  foldl (+) 0 $ map (\(cat, val) -> val) $ scoreSentenceHelper2 words catDataMaps bestCategories

summarize s =
  let words = splitWords s;
      bestCats = bestCategories words;
      sentences = segment s;
      results = map (\sentence -> (sentence, scoreSentenceHelper3 (splitWords sentence) onegrams bestCats)) sentences in
  filter (\(sentence, score) -> score > 5) results
  
main = do     
  let s = "The sport of hocky is about 100 years old by ahdi dates. American Football is a newer sport. Programming is fun. Congress passed a new budget that might help the economy.";
      sentences = segment s;
      rankedCategories = bestCategories (splitWords s);
      categoryNames = map (\(k, v) -> k) rankedCategories
  print $ summarize s
  print rankedCategories
  print categoryNames
  print $ summarize "The acid reaction of the compound. The cat ran."
