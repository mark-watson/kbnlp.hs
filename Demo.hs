import System.IO
import Text.JSON (showJSON, encode)

import Utils

import Categorize

main = do
  s <- getLine
  let cats = bestCategories (splitWords s); 
      bestCat = if length cats > 0 then fst (cats !! 0) else "" in
    do
      putStr "most likely category of text:\t"
      putStrLn bestCat
  main
