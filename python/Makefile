build:
	python setup.py build_ext --inplace

check: build
	pytest

install:
	python setup.py install

.PHONY: build check install
