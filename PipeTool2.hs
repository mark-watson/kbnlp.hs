import System.IO
import Text.JSON (showJSON, encode)

import Utils

import Categorize
import Entities
import Summarize

main = do
  s <- getLine
  let cats = bestCategories (splitWords s); 
      bestCat = if length cats > 0 then fst (cats !! 0) else ""; 
      sum = summarize_s s;
      spwkc = splitWordsKeepCase s;
      result = (bestCat, sum, peopleNames spwkc, countryNames spwkc, companyNames spwkc) in
    print result
  main
