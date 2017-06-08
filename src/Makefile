all: disktest libdht.so

disktest: disktest.o diskhash.o
	gcc -o $@ disktest.o diskhash.o

libdht.so: diskhash.o
	gcc -shared -o $@ $<

diskhash.o: diskhash.c diskhash.h
	gcc -o $@ -Wall -c diskhash.c

disktest.o: disktest.c
	gcc -o $@ -Wall -c $<

.PHONY: all
