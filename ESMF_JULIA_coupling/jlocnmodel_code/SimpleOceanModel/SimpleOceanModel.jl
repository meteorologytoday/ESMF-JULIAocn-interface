if ! ( :ModelTimeManagement in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "ModelTimeManagement.jl"))
end

if ! ( :LogSystem in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "LogSystem.jl"))
end
    
module SimpleOceanModel
    
    using MPI
    using ..ModelTimeManagement
    using ..LogSystem
    
    include(joinpath(@__DIR__, "SimpleOceanModel_CORE.jl"))

    using .SimpleOceanModel_CORE
    
    name = "SimpleOceanModel"

    mutable struct METADATA
        casename    :: AbstractString
        Tile        :: Union{SimpleOceanModel_CORE.Tile, Nothing}
        clock       :: ModelClock

        x2o         :: Union{Dict, Nothing}
        o2x         :: Union{Dict, Nothing}

        config     :: Dict
        recorders   :: Union{Dict, Nothing}
        jdi        :: Any#JobDistributionInfo
        sync_data   :: Dict
    end


    function init(;
        casename     :: AbstractString,
        clock        :: ModelClock,
        config      :: Dict,
        read_restart :: Bool,
    )

        comm = MPI.COMM_WORLD
        rank = MPI.Comm_rank(comm)
        comm_size = MPI.Comm_size(comm)
        
        if comm_size == 1
            throw(ErrorException("Need at least 2 processes to work"))
        end

        is_master = ( rank == 0 )

        my_tile   = nothing
        jdi       = nothing
        recorders = nothing
        sync_data = Dict()
        x2o = Dict()
        o2x = Dict()
        MD = METADATA(
            casename,
            my_tile,
            clock,
            x2o,
            o2x,
            config,
            recorders,
            jdi,
            sync_data,
        )

        return MD

    end

    function run!(
        MD            :: METADATA;
        Δt            :: Int64,
        write_restart :: Bool,
    )

        comm = MPI.COMM_WORLD
        rank = MPI.Comm_rank(comm)
        is_master = rank == 0
        
    end

    function final(MD::METADATA)

        writeLog("Finalizing the model.") 
      
    end

    function writeRestart(
        MD :: METADATA,
    )

    end


    function syncM2S!(
        MD,
    )
        syncField!(
            MD.sync_data[:qflx_direct],
            MD.jdi,
            :M2S,
            :BLOCK,
        ) 

    end    
end
