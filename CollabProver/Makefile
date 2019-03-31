prover: UnifSub.hs Types.hs Namespace.hs ShowTex.hs Main.hs
	ghc Main

test: test.hs prover
	ghc test

clean:
	rm -f *.hi *.o
	rm -f test
	rm -f Main
