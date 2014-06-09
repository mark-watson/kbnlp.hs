module Entities (entities) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sortBy)

import Categorize (splitWords, bigram)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

entities = ""
      
main = do
    let s = "The sport of hocky is about 100 years old by ahdi dates. American Football is a newer sport. Programming is fun. Congress passed a new budget that might help the economy."
    print "Testing NER...."
    print $ S.member "Robert" firstNames
    print $ S.member "Watson" lastNames
    print $ S.member "Mrs" namePrefixes
    print $ S.member "XXXzzz" namePrefixes

    
