{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module Sparql1 where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

import Data.RDF hiding (triple)
import Data.RDF.TriplesGraph
import Data.Text

simpleSelect :: Query SelectQuery
simpleSelect = do
    resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

    x    <- var
    name <- var

    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
    triple x (foaf .:. "name") name

    return SelectQuery { queryVars = [name] }
    
main = do
  s <- selectQuery "http://dbpedia.org/sparql" simpleSelect
  case s of
    Just a -> print $ Prelude.map (\[(Bound (LNode (PlainLL s lan)))] -> s) a
    Nothing -> print "nothing"
