#!/bin/csh -f




mkdir -p build

rm -rf build/*
cp coupledSolver/* build

# build the test coupler
cd coupledSolver

# set the path of ESMF installation
./Allmake.sh
cd ..

# run the test coupler
cd run
./Allrun
