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
      people = encode $ showJSON $ peopleNames spwkc;
      countries = encode $ showJSON $ countryNames spwkc;
      companies = encode $ showJSON $ companyNames spwkc; 
      result = encode $ showJSON [bestCat, sum];
      result2 = encode $ showJSON [people, countries, companies] in
    do
      putStr "category:\t"
      putStrLn bestCat
      putStr "summary:\t"
      putStrLn sum
      putStr "people:\t"
      putStrLn people
      putStr "companies:\t"
      putStrLn companies
      putStr "countries:\t"
      putStrLn countries
  main
