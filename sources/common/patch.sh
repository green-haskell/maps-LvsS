#!/bin/bash


mapImplementation=$1


echo -e "Copying 'Environments.$mapImplementation.hs.template' to 'Environments.hs'"
cp Environments.$mapImplementation.hs.template Environments.hs

echo -e "Copying 'Ops.$mapImplementation.hs.template' to 'Ops.hs'"
cp Ops.$mapImplementation.hs.template Ops.hs

echo -e "Copied"


