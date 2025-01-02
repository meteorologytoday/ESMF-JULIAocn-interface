using Printf
using TOML
@printf("Running %s\n", @__FILE__)


@printf("This program emulate what ESMF is doing, serving as a debugging tool.\n")
@printf("This program also has the potential to serve as a future prototype of Julia centered coupler.\n")


include("../MPITools/MPI_essentials.jl")
include("../Driver_generic.jl")

if !(:CouplingModule in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "Interface", "CouplingModule.jl")))
end

if !(:LogSystem in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "share", "LogSystem.jl")))
end

if !(:SimpleOceanModel in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "SimpleOceanModel", "SimpleOceanModel.jl")))
end


using .LogSystem
using .CouplingModule
using .SimpleOceanModel
using .DriverModule


