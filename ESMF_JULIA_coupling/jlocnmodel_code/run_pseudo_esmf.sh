#!/bin/bash

nproc=2

mpiexec -np $nproc julia pseudo_esmf_SimpleOceanModel.jl


