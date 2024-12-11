#!/bin/bash

nproc=1

mpiexec -np $nproc julia pseudo_esmf_SingleColumnOceanModel.jl


