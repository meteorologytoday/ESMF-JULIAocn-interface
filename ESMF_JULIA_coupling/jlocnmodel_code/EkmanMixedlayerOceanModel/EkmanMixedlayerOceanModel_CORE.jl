#=
if ! ( :ModelTimeManagement in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "ModelTimeManagement.jl"))
end

if ! ( :LogSystem in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "LogSystem.jl"))
end
 
if ! ( :Domains in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "Domains.jl"))
end
 
if ! ( :Config in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "Config.jl"))
end
 
if ! ( :DataManager in names(Main) )
    include(joinpath(@__DIR__, "lib", "DataManager.jl"))
end


if ! ( :Parallelization in names(Main) )
    include(joinpath(@__DIR__, "lib", "Parallelization.jl"))
end

if !(:CyclicData in names(Main))
    include(normpath(joinpath(dirname(@__FILE__), ".", "lib", "CyclicData.jl")))
end

=#

macro hinclude(path)
    return :(include(normpath(joinpath(@__DIR__, $path))))
end

module EkmanMixedlayerOceanModel_CORE

    EMOM = EkmanMixedlayerOceanModel_CORE

    using LinearAlgebra
    using MPI
    using Dates
    using Printf
    using Formatting
    using SharedArrays
    using Distributed
    using SparseArrays
    using NCDatasets
    using JLD2
    using JSON
    using DataStructures

    using ...ModelTimeManagement
    using ...Config
    using ...CyclicData
    using ...LogSystem
    using ...DataManager



    macro hinclude(path)
        return :(include(normpath(joinpath(@__DIR__, $path))))
    end
 
    @hinclude("./lib/constants.jl")
    @hinclude("./lib/ocean_state_function.jl")

    # classes
    @hinclude("./lib/GridFile.jl")
    @hinclude("./lib/PolelikeCoordinate.jl")
    @hinclude("./lib/BasicMatrixOperators.jl")
    @hinclude("./lib/AdvancedMatrixOperators.jl")



    @hinclude("./configs/EMOM_configs.jl")
    @hinclude("./configs/domain_configs.jl")

    @hinclude("VerticalDiffusion.jl")

    @hinclude("Topography.jl")
    @hinclude("Workspace.jl")
    @hinclude("Env.jl")
    @hinclude("TempField.jl")
    @hinclude("Field.jl")
    @hinclude("Core.jl")
    
    @hinclude("ModelBlock.jl")

    @hinclude("Leonard1979.jl")
    @hinclude("setupForcing.jl")
    @hinclude("stepAdvection.jl")
    @hinclude("stepColumn.jl")
    @hinclude("checkBudget.jl")
    
    @hinclude("var_list.jl")
    @hinclude("var_desc.jl")
    
    @hinclude("snapshot_funcs.jl")
    
    @hinclude("updateDatastream.jl")
    @hinclude("updateUVSFC.jl")
    @hinclude("updateBuoyancy.jl")
    @hinclude("updateSfcFlx.jl")

end



