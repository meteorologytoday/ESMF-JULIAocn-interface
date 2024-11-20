#!/bin/bash

loadesmf

echo "JL_INC=$JL_INC"
echo "JL_LIB=$JL_LIB"
echo "MPI_LIB=$MPI_LIB"

set -x
mpicc c_main.c -I$JL_INC -L$JL_LIB -ljulia  -o run_c.exe
