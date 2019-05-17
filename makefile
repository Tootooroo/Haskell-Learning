all:
	stack ghc try.hs
reviewer: reviewer.hs Modules/ConfigReader.hs
	stack ghc reviewer.hs
