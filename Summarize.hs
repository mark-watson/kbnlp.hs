module Summarize (summarize, summarize_s) where

import qualified Data.Map as M
import Data.List.Utils (replace)

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
 
scoreSentenceByBestCategories words catDataMaps bestCategories =
  map (\(category, aMap) -> 
        (category, (safeLookup category bestCategories) * 
                   (scoreSentenceHelper words aMap))) catDataMaps

scoreForSentence words catDataMaps bestCategories =  
  foldl (+) 0 $ map (\(cat, val) -> val) $ scoreSentenceByBestCategories words catDataMaps bestCategories

summarize s =
  let words = splitWords s;
      bestCats = bestCategories words;
      sentences = segment s;
      result1grams = map (\sentence -> (sentence, scoreForSentence (splitWords sentence) onegrams bestCats)) 
                     sentences;
      result2grams = map (\sentence ->
                           (sentence, scoreForSentence (bigram_s (splitWords sentence)) twograms bestCats)) 
                     sentences in
  filter (\(sentence, score) -> score > 100) $
  M.toList $ M.unionWith (+) (M.fromList result1grams) (M.fromList result1grams)
  
summarize_s s =
  let a = replace "\"" "'" $ concat $ map (\x -> (fst x) ++ " ") $ summarize s in
  if length a > 0 then (init a) else safeFirst $ segment s where
    safeFirst x = if length x > 0 then x !! 0 else ""
  
main = do     
  let s = "The U.N. Security Council called Saturday for a cease-fire in the Israeli-Palestinian conflict centered on the Gaza Strip. A council statement approved by all 15 members calls for de-escalation of the violence, restoration of calm, and a resumption of direct negotiations between Israelis and Palestinians aimed at achieving a comprehensive peace agreement based on a two-state solution. The statement calls for \"the reinstitution of the November 2012 cease-fire,\" which was brokered by Egypt, but gives no time frame for when it should take effect."
  print $ summarize s
  print $ summarize_s s
