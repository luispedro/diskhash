all: c python haskell

c:
	cd src && $(MAKE)

python: build_python

haskell:
	stack build

check: check_python check_haskell

check_haskell:
	stack test

build_python:
	python setup.py build_ext --inplace

check_python: build_python
	pytest

install_python:
	python setup.py install

.PHONY: build_python check_python install_python
.PHONY: all c python haskell check check_haskell
