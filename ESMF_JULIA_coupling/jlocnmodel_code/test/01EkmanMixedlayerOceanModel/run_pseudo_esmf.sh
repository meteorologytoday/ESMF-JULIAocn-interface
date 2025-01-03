#!/bin/bash

nproc=3

mpiexec -np $nproc julia pseudo_esmf_EkmanMixedlayerModel.jl


