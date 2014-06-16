{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module DBPedia where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

import Data.RDF hiding (triple)
import Data.RDF.TriplesGraph
import Data.Text


simpleDescribe :: Query DescribeQuery
simpleDescribe = do
    resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
    uri <- describeIRI (resource .:. "Sedona_Arizona")
    return DescribeQuery { queryDescribe = uri }
    

main = do
  (rdfGraph:: TriplesGraph) <- describeQuery "http://dbpedia.org/sparql" simpleDescribe
  --mapM_ print (triplesOf rdfGraph)
  --print "\n\n\n"
  --print rdfGraph
  mapM_ (\(Triple s p o) -> print [s, p, o])
    (triplesOf rdfGraph)
  

  
