all: c python haskell

c:
	cd src && $(MAKE)

python:
	cd python && $(MAKE)

haskell:
	stack build

check: python
	stack test
	cd python && $(MAKE) check

.PHONY: all c python haskell check
