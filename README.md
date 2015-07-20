
Code to process an index.tar.gz file from Hackage,
select out only the latest versions of each cabal file,
parse them, compute their rank and display selected
information about them.

Usage:

  wget http://hackage.haskell.org/packages/index.tar.gz
  parse-cabal index.tar.gz

