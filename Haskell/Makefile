all: sandbox
	cabal install

sandbox:
	cabal sandbox init
	cabal sandbox add-source ~/code/github/haskell-servant/servant/servant-server
	cabal sandbox add-source ~/code/github/haskell-servant/servant/servant-client
	cabal sandbox add-source ~/code/github/haskell-servant/servant/servant

