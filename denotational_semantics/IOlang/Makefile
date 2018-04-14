all: byval byname clean

byval: denotbyval.hs
	ghc -o $@ $^

byname: denotbyname.hs
	ghc -o $@ $^

clean:
	rm *.o *hi 

distclean:
	rm byval byname
