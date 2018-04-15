# File: makefile

.SUFFIXES:
.SUFFIXES: .scm .s .o .exe .c

all: stdio.o mmap.o gc.o


.c.s:
	gcc -O0 -S -o $*.s $*.c

.s.o:
	gcc -c -o $*.o $*.s

test: all
	./run-unit-tests.scm $2
ut: all
	./run-unit-tests.scm

clean:
	rm -f gc.s *.o *.exe *~ unit-tests/*.s unit-tests/*.exe unit-tests/*~
