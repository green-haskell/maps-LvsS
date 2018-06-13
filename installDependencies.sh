#!/bin/bash

ITEMS=(assocCollections coll-Heaps coll-Sets seqs)

function cloneRepo() {

    SECONDS=5

    if [ -d $1 ] ; then

        echo "Error: $1: Repository exists!"
        exit 1

    fi

    echo -e "Cloning repository: '$2', in $SECONDS seconds..." ; sleep $SECONDS

    git clone $2


}


function setupSandboxes() {

    if [ ! -d $1 ] ; then

        echo "Error: $1: Directory does not exist!"
        exit 1

    fi

    cd $1

    if [ ! -d ".cabal-sandbox" ] ; then

        cabal sandbox init

        cabal sandbox add-source ../criterion/


    fi

    cabal install --dependencies-only

    cd ..


}


function main() {

    cloneRepo 'criterion' 'https://github.com/green-haskell/criterion.git'


    setupSandboxes 'maps-LvsS'


}


main





