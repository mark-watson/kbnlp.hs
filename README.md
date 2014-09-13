kbnlp.hs
========

Mark Watson's Haskell NLP Experiments

Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the AGPL version 3 license.

This project also includes some experiments using Haskell for SPARQL clients.

I am currently working on using, when possible, DBPedia URIs as identifiers for entities detected in text. As is much of this project, this is a work in progress.

## Git pull requests welcome!

While I have 30+ years of professional Lisp experience, I am still getting up to speed on using Haskell. So far this has been a 4 year (very much part time) process.

I would very much appreciate git pull requests for:

- Improving my Haskell code
- Fixing bugs in the NLP code and suggested improvements

## Credits

I use the Haskell stemmer written by Dmitry Antonyuk and the sentence splitting code written by Eric Kow.

Thanks!!

## Generated code

There are a fair number of Haskell "source" files that were generated by Ruby scripts that are not included in this project.

These files (e.g., CityNamesDbpedia.hs, Category1Gram.hs, Category2Gram.hs, etc.) mostly use Data.Map.fromList to create in-memory maps for lingusitic and other data. These files are not particularly interesting. The more interesting code is found in the top level files Summarize.hs, Entities.hs, etc.

## required packages:

cabal install yesod

cabal install hsparql   -- also installs MissingH, which is useful!

cabal install json

cabal install encoding

cabal install split

cabal install download-curl

## Running the NLP examples

The file WebApp.hs is a standalone web application that uses much of my experimental NLP code.

Top level utlities like Entities.hs, Summarize.hs, etc. contain main functions so they can be run as is to process example text that is embedded the main test functions.

runghc WebApp.hs

runghc Entities.hs

runghc Summarize.hs

etc.
