FLAGS= -O2 -optc-O2

all: densem_wpp clean


densem_wpp: densem_wpp.hs
	ghc $(FLAGS) -o densem_wpp densem_wpp.hs

clean:
	rm *.hi *.o

distclean:
	rm densem_wpp
