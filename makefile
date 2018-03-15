# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .o .exe

all: stdio.o mmap.o

.s.o:
	gcc -c -o $*.o $*.s

test: ut

ut: all
	./run-unit-tests.scm

clean:
	rm -f *.o *.exe *~ unit-tests/*.s unit-tests/*.exe unit-tests/*~
