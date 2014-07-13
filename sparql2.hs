{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module Sparql2 where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

import Data.RDF hiding (triple)
import Data.RDF.TriplesGraph

simpleDescribe :: Query DescribeQuery
simpleDescribe = do
    resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
    uri <- describeIRI (resource .:. "Sedona_Arizona")
    return DescribeQuery { queryDescribe = uri }
    

doit = do
  (rdfGraph:: TriplesGraph) <- describeQuery "http://dbpedia.org/sparql" simpleDescribe
  --mapM_ print (triplesOf rdfGraph)
  --print "\n\n\n"
  --print rdfGraph
  mapM (\(Triple s p o) -> 
          case [s,p,o] of
            [UNode(s), UNode(p), UNode(o)] -> return (s,p,o)
            [UNode(s), UNode(p), LNode(PlainLL o2 l)] -> return (s,p,o2)
            [UNode(s), UNode(p), LNode(TypedL o2 l)] -> return (s,p,o2)
            _ -> return ("no match","no match","no match"))

    (triplesOf rdfGraph)

          
main = do
  results <- doit
  print $ results !! 0
  mapM_ print results
  return results

