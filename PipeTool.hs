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
      result = encode $ showJSON [bestCat, sum] in
    putStrLn result
         