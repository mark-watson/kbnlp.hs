{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module Combo where

import Sparql2 (doit)
import OpenCalais (calaisResults)

main = do
  c <- calaisResults "Berlin Germany visited by George W. Bush to see IBM plant. Bush met with President Clinton. Bush said “felt it important to crank up”"
  s <- doit
  print c
  print $ c !! 0
  print $ c !! 1
  print s
  print $ s !! 0
  print $ s !! 1
  
