#!/bin/bash

version=3.13

fullpy=python${version}
PY_INC=/home/t2hsu/miniconda3/envs/mpi/include/$fullpy
PY_LIB=/home/t2hsu/miniconda3/envs/mpi/lib/$fullpy

echo "PY_INC=$PY_INC"
echo "PY_LIB=$PY_LIB"

set -x
mpicc main.c -I$PY_INC -L$PY_LIB -l$fullpy -o run.exe
