CC=gcc
LOGFLAG=-DLOG
DEBUGFLAG=-DDBG
HFLAGS=${LOGFLAG} ${DEBUGFLAG}
CFLAGS=-Wall -Wextra -Wno-unused-result -Wno-unused-label -g -Wno-unused-variable -O3 -march=native #${DEBUGFLAG} # ${LOGFLAG}

all: stack-gc.o vm-gc.o vm-gc clean

stack-gc.o: stack-gc.c stack-gc.h

vm-gc.o: vm-gc.c stack-gc.h vm.h

vm-gc: stack-gc.o vm-gc.o 

clean:
	rm *.o

distclean:
	rm vm-gc
