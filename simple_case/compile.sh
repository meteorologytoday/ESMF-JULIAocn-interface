#!/bin/bash

echo "JL_INC=$JL_INC"
echo "JL_LIB=$JL_LIB"

gcc -c c_code.c -I$JL_INC -o c_code.o
gfortran -c test.f90 -o test.o

gfortran test.o c_code.o -L$JL_LIB -ljulia -o test.exe
