#!/bin/bash

loadesmf

MPI_LIB=/cm/shared/apps/spack/0.17.3/cpu/b/opt/spack/linux-rocky8-zen2/gcc-10.2.0/openmpi-4.1.3-oq3qvsvt5mywjzy7xzrfeh6eebiujvbm/lib

echo "JL_INC=$JL_INC"
echo "JL_LIB=$JL_LIB"
echo "MPI_LIB=$MPI_LIB"

set -x
g++ -c c_gate.c -I$JL_INC  -o c_gate.o
mpif90 -c main.f90 -I$MPI_LIB -o main.o

mpif90 main.o c_gate.o -L$JL_LIB -ljulia -o run.exe
