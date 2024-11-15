dev:
	cabal exec site clean && cabal build && cabal exec site build && cabal exec site watch
