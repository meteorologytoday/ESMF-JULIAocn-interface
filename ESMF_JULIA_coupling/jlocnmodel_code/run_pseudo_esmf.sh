#!/bin/bash

nproc=5

mpiexec -np $nproc julia pseudo_esmf.jl


