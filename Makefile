cabal:
	cabal clean
	cabal install
dev:
	ghc -o yadic yadic.hs
