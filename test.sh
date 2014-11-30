cabal install && \
   { cat tests/simple | .cabal-sandbox/bin/m5 $@ ; }

