FLAGS = -O2 -optc-O2
all: treepbt bird


treepbt: TreePBT.hs
	ghc $(FLAGS) -o treepbt TreePBT.hs

bird: Bird.hs
	ghc $(FLAGS) -o bird Bird.hs

clean:
	rm *.o *.hi 

distclean:
	rm treepbt bird
