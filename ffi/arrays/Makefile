CC=gcc
CFLAGS=-I.

OBJ = array.o

%.o: %.c
	$(CC) -c -g3 -Wall -Werror -fpic -o $@ $< $(CFLAGS)

all: libarray.so hprog

libarray.so: array.o
	gcc -shared -o libarray.so array.o

hprog: libarray.so
	hsc2hs hprog.hsc
	ghc -o hprog hprog.hs -larray -L.

.PHONY: clean

clean:
	rm -f *.o *.hi *~ core *.so hprog hprog.hs

