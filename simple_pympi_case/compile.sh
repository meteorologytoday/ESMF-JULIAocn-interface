#!/bin/bash

version=3.10

PY_ROOT=/home/t2hsu/miniconda3/envs/mpi

fullpy=python${version}
INC_FLAGS="-I$PY_ROOT/include/$fullpy -I$PY_ROOT/lib/python3.10/site-packages/mpi4py/include/" 
LIB_FLAGS="-L$PY_ROOT/lib"
LD_FLAGS="-l$fullpy"

set -x
mpicc main.c $INC_FLAGS $LIB_FLAGS $LD_FLAGS -o run.exe
