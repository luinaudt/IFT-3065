# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .o .exe .c

all: stdio.o mmap.o gc.o


.c.o:
	gcc -O0 -c -o $*.o $*.c

.s.o:
	gcc -c -o $*.o $*.s

test: all
	./run-unit-tests.scm $2
ut: all
	./run-unit-tests.scm

clean:
	rm -f *.o *.exe *~ unit-tests/*.s unit-tests/*.exe unit-tests/*~
