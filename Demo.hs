-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

-- this is a throw away test file

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
