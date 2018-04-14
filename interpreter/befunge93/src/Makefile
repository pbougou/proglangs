.phony: all clean distclean
all: befunge.o befungevm.o bfvm abstract_control.o abs_control indirect_threaded.o direct_threaded.o direct_threaded indirect_threaded prefetching.o prefetching 
	# DBG_FLAG: Compiling with this flag on, every unsupported befunge93 command gives error message.
	# Otherwise, unsupported commands are considered as comments. If program makes sense we keep it.
CC=gcc
CFLAGS=-O3 -Wno-format -Wno-unused-variable -Wno-unused-result 
DFLAG=-DDBG_FLAG

befunge.o: befunge.c
	$(CC) $(CFLAGS) -c befunge.c 

befungevm.o: befungevm.c
	$(CC) $(CFLAGS) -c befungevm.c

befungevm_fns.o: befungevm_fns.c
	$(CC) $(CFLAGS) -c befungevm_fns.c

bfvm: befunge.o befungevm.o befungevm_fns.o
	$(CC) $(CFLAGS) -o bfvm befunge.o befungevm.o befungevm_fns.o


abstract_control.o: abstract_control.c
	$(CC) $(CFLAGS) -c abstract_control.c

abs_control: befunge.o abstract_control.o befungevm_fns.o
	$(CC) $(CFLAGS) -o abs_control befunge.o abstract_control.o befungevm_fns.o


indirect_threaded.o: indirect_threaded.c
	$(CC) $(CFLAGS) -c indirect_threaded.c

indirect_threaded: befunge.o indirect_threaded.o befungevm_fns.o
	$(CC) $(CFLAGS) -o indirect_threaded befunge.o indirect_threaded.o befungevm_fns.o

direct_threaded.o: direct_threaded.c
	$(CC) $(CFLAGS) -c direct_threaded.c

direct_threaded: direct_threaded.o befungevm_fns.o befunge.o
	$(CC) $(CFLAGS) -o direct_threaded befunge.o direct_threaded.o befungevm_fns.o 

prefetching.o: prefetching.c
	$(CC) $(CFLAGS) -c prefetching.c

prefetching: prefetching.o befungevm_fns.o befunge.o
	$(CC) $(CFLAGS) -o prefetching prefetching.o befungevm_fns.o befunge.o

clean: 
	rm *.o

asmclean:
	rm *.s

distclean:
	rm bfvm abs_control indirect_threaded direct_threaded prefetching
