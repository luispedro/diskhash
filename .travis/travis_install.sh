#!/usr/bin/env bash


if test -e $HOME/miniconda/envs/condaenv; then
    echo "condaenv already exists"
else
    conda create  --yes -n condaenv python=$TRAVIS_PYTHON_VERSION pytest
fi

source activate condaenv
stack --no-terminal setup
make
