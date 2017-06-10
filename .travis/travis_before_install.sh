#!/usr/bin/env bash

mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH

if test -e $HOME/.local/bin/stack; then
    echo "stack already installed"
else
    curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi

if test -e $HOME/miniconda/bin; then
    echo "miniconda already installed."
else
    rm -rf $HOME/miniconda
    wget http://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh -O miniconda.sh
    chmod +x miniconda.sh
    ./miniconda.sh -b -p $HOME/miniconda
fi
export PATH=$HOME/miniconda/bin:$PATH
conda update --yes conda

