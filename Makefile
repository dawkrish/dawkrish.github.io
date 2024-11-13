dev:
	cabal build && cabal exec site rebuild && cabal exec site watch