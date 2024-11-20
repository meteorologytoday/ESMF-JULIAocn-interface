#!/bin/bash

JL_INC=/home/t2hsu/julia/julia-1.9.3/include/julia
JL_LIB=/home/t2hsu/julia/julia-1.9.3/lib


echo "JL_INC=$JL_INC"
echo "JL_LIB=$JL_LIB"

set -x
mpicc c_main.c -I$JL_INC -L$JL_LIB -ljulia  -o run_c.exe
