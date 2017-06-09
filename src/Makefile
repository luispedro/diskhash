all: disktest libdht.so

disktest: disktest.o diskhash.o
	gcc $(CFLAGS) -o $@ disktest.o diskhash.o

libdht.so: diskhash.o
	gcc -shared $(CFLAGS) -o $@ $<

diskhash.o: diskhash.c diskhash.h
	gcc $(CFLAGS) -fPIC -o $@ -Wall -c diskhash.c

disktest.o: disktest.c diskhash.h
	gcc $(CFLAGS) -o $@ -Wall -c $<

.PHONY: all
