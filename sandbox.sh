cabal sandbox init
cabal sandbox add-source $MY_SRC/hs-minu
cabal install --only-dependencies
