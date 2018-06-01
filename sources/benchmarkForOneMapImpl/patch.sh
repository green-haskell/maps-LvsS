#!/bin/bash


mapImplementation=$1


echo -e "Patching 'Main.hs.template' into 'Main.hs'"

sed "s/--import qualified <MapImplementation> as M/import qualified $mapImplementation as M/;s/\"<MapImplementation>\"/\"$mapImplementation\"/" ./Main.hs.template > ./Main.hs

echo -e "Patched..."


