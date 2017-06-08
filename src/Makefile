all: disktest libdht.so

disktest: disktest.o diskhash.o
	gcc $(CFLAG)S -o $@ disktest.o diskhash.o

libdht.so: diskhash.o
	gcc -shared $(CFLAG)S -o $@ $<

diskhash.o: diskhash.c diskhash.h
	gcc $(CFLAG)S -o $@ -Wall -c diskhash.c

disktest.o: disktest.c diskhash.h
	gcc $(CFLAG)S -o $@ -Wall -c $<

.PHONY: all
