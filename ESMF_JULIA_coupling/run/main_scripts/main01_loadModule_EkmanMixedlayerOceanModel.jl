using Printf
@printf("Running %s\n", @__FILE__)
if !(:SimpleOceanModel in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "EkmanMixedlayerOceanModel", "EkmanMixedlayerOceanModel.jl")))
end

OMMODULE = EkmanMixedlayerOceanModel
config_file = "config_EkmanMixedlayerOceanModel.toml"

