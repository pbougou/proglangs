all: tyinf-sm clean

read_tyinf: read-typeinfer.hs
	ghc read-typeinfer.hs

tyinf-sm: tyinf-sm.hs
	ghc -o tyinf-sm tyinf-sm.hs

clean:
	rm *.o *hi 

distclean:
	rm tyinf-sm
