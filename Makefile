euler: euler.hs
	ghc euler.hs -Odph -fvia-C -optc-O3 -optc-march=native -fexcess-precision --make
	./euler

clean:
	rm -vf *.hi *.o euler

