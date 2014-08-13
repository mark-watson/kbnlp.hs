kbnlp.hs
========

Haskell NLP Work

## installed packages:

cabal install yesod
cabal install hsparql   -- also installs MissingH, which is useful!
cabal install json
cabal install encoding
cabal install split
--cabal install control-monad-exception
--cabal install explicit-exception
--cabal install enumerator
--cabal install http-enumerator
cabal install download-curl

## Performance tuning

An example:

ghc -O2 --make Entities.hs -prof -auto-all
time ./Entities +RTS -p

Look in generated file Entities.prof

