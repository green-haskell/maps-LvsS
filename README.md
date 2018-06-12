git clone https://github.com/green-haskell/criterion.git

git clone https://github.com/green-haskell/maps-LvsS.git

cd maps-LvsS

cabal sandbox init

cabal sandbox add-source ../criterion/

cabal install --dependencies-only

\#To run: as root, in the maps-LvsS directory:

make 2>&1 | tee fullOutput.txt
