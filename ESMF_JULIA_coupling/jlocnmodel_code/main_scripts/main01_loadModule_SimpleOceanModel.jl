using Printf
@printf("Running %s\n", @__FILE__)
if !(:SimpleOceanModel in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "SimpleOceanModel", "SimpleOceanModel.jl")))
end

OMMODULE = SimpleOceanModel
config_file = "config_SimpleOceanModel.toml"

