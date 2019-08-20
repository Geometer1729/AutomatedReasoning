prover: 
	ghc -Wall Main

test: test.hs prover
	ghc test

clean:
	rm -f *.hi *.o
	rm -f test
	rm -f Main
