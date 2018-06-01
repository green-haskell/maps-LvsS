#!/bin/bash

mapImplementations=(Data.HashMap.Lazy Data.HashMap.Strict Data.IntMap.Lazy Data.IntMap.Strict Data.Map.Lazy Data.Map.Strict)


## ##########

#RED='\033[1;31m'
#NC='\033[0m' # No Color
RED=
NC=


## ##########

function patchFilesForMapImplementation () {

    echo -e "    ${RED}Started patching files for '$1'${NC}\n"

    cd ./sources/benchmarkForOneMapImpl/ ; sh ./patch.sh $mapImplementation ; cd ../..

    cd ./sources/common/ ; sh ./patch.sh $mapImplementation ; cd ../..
 
    echo -e "    ${RED}Finished patching files for '$1'${NC}\n"


}


## ##########

function main () {

    for mapImplementation in "${mapImplementations[@]}"
    do

        echo -e "\n\nProcessing "$mapImplementation"\n\n"
        date

patchFilesForMapImplementation "$mapImplementation"

        cabal configure
        cabal build compileForAllMapImplementations
        cp mapImplementations/$mapImplementation.txt ./mapImplementations.txt
        mkdir -p tmp && mkdir -p tmp/executables
        ./dist/build/compileForAllMapImplementations/compileForAllMapImplementations
        cabal configure
        cabal build runForAllBenchmarkTypes
        modprobe msr
        ./dist/build/runForAllBenchmarkTypes/runForAllBenchmarkTypes
        chown $USER:users time-Time.csv packageEnergy-Energy.csv dramEnergy-Energy.csv
        mkdir -p csvFileSets
        mv time-Time.csv csvFileSets/$mapImplementation.time.csv
        mv packageEnergy-Energy.csv csvFileSets/$mapImplementation.packageEnergy.csv
        mv dramEnergy-Energy.csv csvFileSets/$mapImplementation.dramEnergy.csv
        date

        rm -fr dist/
        rm sources/benchmarkForOneMapImpl/Main.hs
        rm sources/common/Environments.hs
        rm sources/common/Ops.hs
        rm mapImplementations.txt


    done

    #rm sources/common/Defs.hs


}


## ##########

main


