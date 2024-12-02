#!/bin/bash

output=a.out

rm -rf $output

echo "Compile..."

esmf_shared=/home/t2hsu/miniconda3/envs/mpi

gcc main.c -DESMF_PIO=1 -I${esmf_shared}/include -L${esmf_shared}/lib -L${esmf_shared}/lib/mod -lnetcdf -lnetcdff -lpiof -lesmf -o a.out


if [ -f "$output" ]  ; then
    echo "Running $output"
    ./a.out
else
    echo "Target output file $output does not exists."
    exit 1
fi
