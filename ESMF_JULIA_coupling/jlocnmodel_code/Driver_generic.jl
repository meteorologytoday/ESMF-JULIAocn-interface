using JLD2
using MPI
using Printf

if !(:LogSystem in names(Main))
    include(normpath(joinpath(dirname(@__FILE__), "share", "LogSystem.jl")))
end

if !(:ModelTimeManagement in names(Main))
    include(normpath(joinpath(dirname(@__FILE__), "share", "ModelTimeManagement.jl")))
end

if !(:appendLine in names(Main))
    include(normpath(joinpath(dirname(@__FILE__), "share", "AppendLine.jl")))
end
 
if !(:CouplingModule in names(Main))
    include(normpath(joinpath(@__DIR__, "Interface", "CouplingModule.jl")))
end

if !(:Config in names(Main))
    include(normpath(joinpath(@__DIR__, "share", "Config.jl")))
end

if !(:Domains in names(Main))
    include(normpath(joinpath(@__DIR__, "share", "Domains.jl")))
end


include(normpath(joinpath(dirname(@__FILE__), "configs", "driver_configs.jl")))

module DriverModule

    using MPI
    using Printf
    using ..LogSystem
    using ..CouplingModule
    using ..ModelTimeManagement
    using ..Config
    using ..Domains
   
    include("configs/driver_configs.jl")
 

    mutable struct Driver
        
        config   :: Union{Dict, Nothing}
        OMMODULE :: Module
        OMDATA   :: Any
        comm     :: MPI.Comm
        ci       :: CouplingModule.CouplingInterface
        misc     :: Union{Dict, Nothing}
        log_handle :: LogHandle
        domain     :: Union{ Domains.Domain, Nothing}

        function Driver(
            config   :: Dict,
            OMMODULE :: Module,
            comm     :: MPI.Comm,
            ci       :: CouplingModule.CouplingInterface,
            log_handle :: LogHandle,
        )

            return new(
                config,
                OMMODULE,
                nothing,
                comm,
                ci,
                Dict(),
                log_handle,
                nothing,
            )

        end
    end


    function initModel!(
        dr :: Driver,
    )

        comm = dr.comm
        rank = MPI.Comm_rank(comm)
        ntask = MPI.Comm_size(comm)
        config = dr.config
        log_handle = dr.log_handle 


        writeLog(log_handle, "===== [ Master Created ] =====")
        writeLog(log_handle, "Number of total tasks       : %d", ntask)
        writeLog(log_handle, "Number of total worker tasks: %d", ntask-1)
        
        MPI.Barrier(comm)

        dr.misc[:is_master] = rank == 0

        is_master = dr.misc[:is_master]

        if is_master
            if config == nothing
                throw(ErrorException("Master thread needs to provide config parameter."))
            end

            writeLog(log_handle, "Validate driver config.")
            config["DRIVER"] = validateConfigEntries(
                config["DRIVER"],
                getDriverConfigDescriptors()["DRIVER"]
            )
            
            coupler_funcs = dr.ci.cpl_funcs
        end

        writeLog(log_handle, "Broadcast config to slaves.")
        config = MPI.bcast(config, 0, comm) 


        p = config["DRIVER"]["caserun"]
        writeLog(log_handle, "Setting working directory to %s", p)
        if is_master
            if ! isdir(p)
                writeLog(log_handle, "Working directory does not exist. Create it.")
                mkpath(p)
            end
        end
        MPI.Barrier(comm)
        cd(p)


        local mt_start = nothing
        local mt_end   = nothing
        local read_restart = nothing
        local dt = nothing

     
        if is_master

            writeLog(log_handle, "Getting model start time.")
            
            read_restart  = config["BASIC"]["restart"]
            iter_start    = config["BASIC"]["iter_start"]
            iter_advance  = config["BASIC"]["iter_advance"]
            modeltime_ref = config["BASIC"]["modeltime_ref"]
            dt            = Int64(config["BASIC"]["dt_num"]) // Int64(config["BASIC"]["dt_den"])
            
             
            if read_restart

                writeLog(log_handle, "read_restart is true.")

                restart_info = JLD2.load("model_restart.jld2")
                restart_t_start = restart_info["timestamp"]

                if t_start == nothing
                    writeLog(log_handle, "t_start == nothing. Skip checking restart time.")
                elseif t_start != restart_t_start
                    throw(ErrorException(
                        @sprintf("Model restart time inconsistent! The start time received from the coupler is %s while the restart file is %s.",
                        Dates.format(t_start, "yyyy-mm-dd HH:MM:SS"),
                        Dates.format(restart_t_start, "yyyy-mm-dd HH:MM:SS"),
                    )))
                else
                    writeLog(log_handle, "Model restart time is consistent.")
                end

            end

            mt_start = ModelTime(
                ModelTimeConfig(modeltime_ref, dt),
                iter_start 
            )

            mt_end = copy_partial(mt_start)
            mt_end.iter = mt_start.iter + iter_advance

            writeLog(log_handle, "### Simulation time start : %s", string(mt_start))
            writeLog(log_handle, "### Simulation time end   : %s", string(mt_end))
             
        end

        writeLog(log_handle, "Broadcast mt_start and read_restart to slaves.")
        mt_start = MPI.bcast(mt_start, 0, comm) 
        mt_end   = MPI.bcast(mt_end, 0, comm) 
        read_restart = MPI.bcast(read_restart, 0, comm) 

        # copy the start time by adding 0 seconds
        beg_datetime = copy_partial(mt_start)

        # Construct model clock
        clock = ModelClock("Model", beg_datetime)

            
        local domain   = nothing
        if is_master
            _cfg = config["DOMAIN"]
            domain = Domains.Domain(
                0, 0,
                _cfg["sNx"],
                _cfg["sNy"],
                _cfg["OLx"],
                _cfg["OLy"],
                _cfg["nSx"],
                _cfg["nSy"],
                _cfg["nPx"],
                _cfg["nPy"],
                _cfg["Nx"],
                _cfg["Ny"],
                _cfg["Nz"],
                nothing,
                nothing,
            )


        end
        domain   = MPI.bcast(domain, 0, comm) 
        Domains.setDomain!(domain; rank=rank, number_of_pet=ntask)
        println("Domain: ", domain)
        dr.domain = domain

        # initializing
        writeLog(log_handle, "==================================")
        writeLog(log_handle, "===== INITIALIZING MODEL: %s =====", dr.OMMODULE.name)
        
        dr.OMDATA = dr.OMMODULE.init(
            casename     = config["DRIVER"]["casename"],
            clock        = clock,
            config       = config,
            read_restart = read_restart,
            comm         = comm,
            log_handle   = log_handle,
        )


        if is_master

            # This is where we establish the array passing between
            # the ocean model and the coupler
            # coupler_funcs.master_after_model_init!(dr)

            # By design, CESM ends the simulation of month m after the run of 
            # the first day of (m+1) month. For example, suppose the model 
            # run for Jan and Feb, the restart file will be written after stepping
            # March 1. This means, the read_restart phase is an already done step.
            # Therefore, after the read_restart phase, we need to advance the time.
            if read_restart
                writeLog(log_handle, "read_restart is true.")
                writeLog(log_handle, "Current time: %s", clock2str(clock))
                advanceClock!(clock, Δt)
                dropRungAlarm!(clock)
            end

        end

        # =======================================
        # IMPORTANT: need to sync time
        _model_time = MPI.bcast(clock.model_time, 0, comm) 
        if !is_master
            setClock!(clock, _model_time.iter)
        end
        # =======================================

        dr.misc[:mt_start] = mt_start
        dr.misc[:mt_end]   = mt_end
        
    end

    function runModel!(
        dr :: Driver;
        write_restart :: Bool,
    )
        log_handle = dr.log_handle
        writeLog(log_handle, "Enter runMmodel.")
        
        is_master = dr.misc[:is_master]
        clock = dr.OMDATA.clock
        config = dr.config
        comm = dr.comm
 
        if is_master 
            writeLog(log_handle, "Current time: %s", clock2str(clock))
        end

        cost = @elapsed let

            # This advanced option is needed when deriving
            # qflux in direct method: Coupler needs to load
            # the initial data of each day and force master
            # to sync thermo variables with slaves. 
            #if config["DRIVER"]["compute_QFLX_direct_method"]
            #    OMMODULE.syncM2S!(OMDATA)
            #end

            # Do the run and THEN advance the clock

            dr.OMMODULE.run!(
                dr.OMDATA;
                niters = 1,
                write_restart = write_restart,
            )
            MPI.Barrier(comm)
            
        end

        writeLog(log_handle, "Computation cost: %.2f secs.", cost)

        if write_restart && is_master

            driver_restart_file = "model_restart.jld2"
            writeLog(log_handle, "Writing restart time of driver: %s", driver_restart_file)
            JLD2.save(driver_restart_file, "timestamp", clock.time)

            archive_list_file = joinpath(
                config["DRIVER"]["caserun"],
                config["DRIVER"]["archive_list"],
            )

            timestamp_str = @sprintf(
                "%s-%05d",
                Dates.format(clock.time, "yyyy-mm-dd"),
                floor(Int64, Dates.hour(clock.time)*3600+Dates.minute(clock.time)*60+Dates.second(clock.time)),
            )

            appendLine(archive_list_file,
                @sprintf("cp,%s,%s,%s",
                    driver_restart_file,
                    config["DRIVER"]["caserun"],
                    joinpath(config["DRIVER"]["archive_root"], "rest", timestamp_str)
                )
            ) 

        end

        #if config["DRIVER"]["compute_QFLX_direct_method"]
        #    writeLog(log_handle, "compute_QFLX_direct_method is true")
        #    OMMODULE.syncM2S!(OMDATA)
        #end


        # ==========================================
        if is_master
            # Advance the clock AFTER the run
            advanceClock!(clock, 1)
            dropRungAlarm!(clock)
        end
       
        # Broadcast time to workers. Workers need time
        # because datastream needs time interpolation. 
        _model_time = MPI.bcast(clock.model_time, 0, comm) 
        if !is_master
            setClock!(clock, _model_time.iter)
        end
        
    end

    function finalizeModel(
        dr :: Driver,
    )
        log_handle = dr.log_handle
        writeLog(log_handle, "Enter runMmodel.")
        
        is_master = dr.misc[:is_master]
        config = dr.config 
        if is_master
           
            dr.OMMODULE.final(dr.OMDATA)
            
            archive(joinpath(
                config["DRIVER"]["caserun"],
                config["DRIVER"]["archive_list"],
            ), log_handle = log_handle)

        end
     
        writeLog(log_handle, "Program Ends.")

    end

    function archive(
        archive_list_file :: String;
        log_handle :: LogHandle, 
    )

        writeLog(log_handle, "===== Archiving files BEGIN =====")
       
        if isfile(archive_list_file)

            writeLog(log_handle, "Archive file list exists: %s\n", archive_list_file)
 
            for line in eachline(archive_list_file)

                args = split(line, ",")

                if length(args) == 0
                    continue
                end
              
                action = args[1]
                args = args[2:end]

                if action in ["mv", "cp"]

                    fname, src_dir, dst_dir = args

                    if ! isdir(dst_dir)
                        mkpath(dst_dir)
                    end
         
                    src_file = joinpath(src_dir, fname)
                    dst_file = joinpath(dst_dir, fname)

                    if isfile(src_file)

                        if action == "mv"
                            mv(src_file, dst_file, force=true)
                            writeLog(log_handle, "Moving file: %s ( %s => %s )", fname, src_dir, dst_dir)
                        elseif action == "cp"
                            cp(src_file, dst_file, force=true)
                            writeLog(log_handle, "Copying file: %s ( %s => %s )", fname, src_dir, dst_dir)
                        end

                    else
                        println("File does not exist: ", src_file)
                    end

                elseif action == "rm"
                    fname, fdir = args
                    rm(joinpath(fdir, fname), force=true)
                    writeLog(log_handle, "Removing file: %s in %s", fname, fdir)
                else
                    throw(ErrorException(@sprintf("Unknown action in archive list: %s", action)))
                end

            end
        else
            writeLog(log_handle, "Archive file list does not exist: %s\n", archive_list_file)
        end
        writeLog(log_handle, "===== Archiving files END =====")

    end

    function getDomainInfo(dr :: Driver, arr :: Vector{Int64})
    #function getDomainInfo()
    
        #println("[DRIVER] Enter getDomainInfo")

        domain = dr.domain
         
        # For now we only support nSx = nSy = 1 
        myXGlobalLo = domain.myXGlobalLo[1]
        myYGlobalLo = domain.myYGlobalLo[1]
    
        assign_arr = [
                domain.sNx,
                domain.sNy,
                domain.OLx,
                domain.OLy,
                domain.nSx,
                domain.nSy,
                domain.nPx,
                domain.nPy,
                domain.Nx,
                domain.Ny,
                domain.Nz,
                myXGlobalLo,
                myYGlobalLo,
        ]

        for (i, d) in enumerate(assign_arr)
            arr[i] = d
        end

        #println("[DRIVER] Leaving getDomainInfo")
    end 
end
