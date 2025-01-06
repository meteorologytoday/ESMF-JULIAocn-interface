#!/bin/bash

nproc=4

mpiexec -np $nproc julia pseudo_esmf_EkmanMixedlayerModel.jl


