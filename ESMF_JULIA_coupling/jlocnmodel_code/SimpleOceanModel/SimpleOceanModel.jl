if ! ( :ModelTimeManagement in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "ModelTimeManagement.jl"))
end

if ! ( :LogSystem in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "LogSystem.jl"))
end
 
if ! ( :Config in names(Main) )
    include(joinpath(@__DIR__, "..", "share", "Config.jl"))
end
    
module SimpleOceanModel
    
    using MPI
    using ..ModelTimeManagement
    using ..LogSystem
    using ..Config
    
    include(joinpath(@__DIR__, "SimpleOceanModel_CORE.jl"))
    
    include(joinpath(@__DIR__, "domain_configs.jl"))
    include(joinpath(@__DIR__, "model_configs.jl"))

    using .SimpleOceanModel_CORE
    
    name = "SimpleOceanModel"

    mutable struct METADATA
        casename    :: AbstractString
        Tile        :: Union{SimpleOceanModel_CORE.Tile, Nothing}
        clock       :: ModelClock

        x2o         :: Union{Dict, Nothing}
        o2x         :: Union{Dict, Nothing}

        config      :: Dict
        recorders   :: Union{Dict, Nothing}
        jdi         :: Any#JobDistributionInfo
        sync_data   :: Dict
        comm        :: MPI.Comm
        log_handle  :: LogHandle
    end


    function init(;
        casename     :: AbstractString,
        clock        :: ModelClock,
        config      :: Dict,
        read_restart :: Bool,
        comm         :: MPI.Comm,
        log_handle   :: LogHandle,
    )
        
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
            comm,
            log_handle,
        )

        return MD

    end

    function run!(
        MD            :: METADATA;
        niters        :: Int64,
        write_restart :: Bool,
    )
        log_handle = MD.log_handle
        comm = MD.comm
        rank = MPI.Comm_rank(comm)
        is_master = rank == 0
        
    end

    function final(MD::METADATA)
        log_handle = MD.log_handle
        writeLog(log_handle, "Finalizing the model.") 
    end

    function writeRestart(
        MD :: METADATA,
    )
        log_handle = MD.log_handle

    end


    function syncM2S!(
        MD,
    )
        log_handle = MD.log_handle
        syncField!(
            MD.sync_data[:qflx_direct],
            MD.jdi,
            :M2S,
            :BLOCK,
        ) 

    end    
end
