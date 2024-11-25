#!/bin/bash

JL_ROOT=~/julia/julia-1.9.3/
JL_INC=$JL_ROOT/include/julia
JL_LIB=$JL_ROOT/lib


echo "JL_INC=$JL_INC"
echo "JL_LIB=$JL_LIB"

set -x
mpicc c_main.c -I$JL_INC -L$JL_LIB -ljulia  -o run_c.exe
