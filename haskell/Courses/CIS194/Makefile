all:
	cabal sandbox init
	cabal install

tests:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build
	cabal test

docs:
	cabal haddock --hyperlink-source
