CC=gcc
CFLAGS=-Wall -Wextra -Wno-unused-result -Wno-unused-label -g 
LOGFLAG=-DLOG

all: stack.o vm.o vm clean

stack.o: stack.c stack.h

vm.o: vm.c stack.h

vm: stack.o vm.o 

clean:
	rm *.o

distclean:
	rm vm
