using Printf
using TOML
@printf("Running %s\n", @__FILE__)


@printf("This program emulate what ESMF is doing, serving as a debugging tool.\n")
@printf("This program also has the potential to serve as a future prototype of Julia centered coupler.\n")


include("../MPITools/MPI_essentials.jl")
include("../Interface/CouplingModule.jl")
include("../SimpleOceanModel/SimpleOceanModel.jl")
include("../Driver_generic.jl")

using .CouplingModule
using .SimpleOceanModel
using .DriverModule


