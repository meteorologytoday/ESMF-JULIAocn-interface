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

module EkmanMixedlayerOceanModel

    using Printf
    using NCDatasets
    using JSON
    using Dates
    using MPI

    using ..ModelTimeManagement
    using ..LogSystem
    using ..Config
    using ..DataManager    
    using ..Domains
    using ..Parallelization

    include(joinpath(@__DIR__, "lib", "AppendLine.jl"))
    include(joinpath(@__DIR__, "EkmanMixedlayerOceanModel_CORE.jl"))
    include(joinpath(@__DIR__, "configs", "domain_configs.jl"))
    include(joinpath(@__DIR__, "configs", "EMOM_configs.jl"))

    using .EkmanMixedlayerOceanModel_CORE
    EMOM = EkmanMixedlayerOceanModel_CORE
 
    name = "EkmanMixedlayerOceanModel"

    mutable struct METADATA
        casename    :: AbstractString

        mb_full     :: Union{EMOM.ModelBlock, Nothing}
        mb          :: Union{EMOM.ModelBlock, Nothing}

        clock       :: ModelClock

        x2o         :: Union{Dict, Nothing}
        o2x         :: Union{Dict, Nothing}

        config      :: Dict
        recorders   :: Union{Dict, Nothing}
        jdi         :: JobDistributionInfo
        sync_data_full   :: Union{Dict, Nothing}
        sync_data   :: Dict
        comm        :: MPI.Comm
        is_master   :: Bool
        log_handle  :: LogHandle
        domain      :: Domains.Domain
    end


    function init(;
        casename     :: AbstractString,
        clock        :: ModelClock,
        config      :: Dict,
        read_restart :: Bool,
        comm         :: MPI.Comm,
        is_master    :: Bool,
        log_handle   :: LogHandle,
    )
        
        rank = MPI.Comm_rank(comm)
        comm_size = MPI.Comm_size(comm)
        cfgs = config
        
        if comm_size == 1
            throw(ErrorException("Need at least 2 processes to work"))
        end

        local master_mb = nothing
        local master_ev = nothing

        if is_master
            archive_list_file = joinpath(config["DRIVER"]["caserun"], config["DRIVER"]["archive_list"])
            if isfile(archive_list_file)
                writeLog(log_handle, "File %s already exists. Remove it.", archive_list_file)
                rm(archive_list_file)
            end
        end

        if is_master

            cfg_desc = EMOM.getEMOMConfigDescriptors()
            cfg_domain_desc = EMOM.getDomainConfigDescriptors()

            misc_config = EMOM.validateConfigEntries(config["MODEL_MISC"], cfg_desc["MODEL_MISC"], log_handle=log_handle)
            core_config = EMOM.validateConfigEntries(config["MODEL_CORE"], cfg_desc["MODEL_CORE"], log_handle=log_handle)
            domain_config = EMOM.validateConfigEntries(config["DOMAIN"], cfg_domain_desc["DOMAIN"], log_handle=log_handle)

            # If `read_restart` is true then read restart file: config["rpointer_file"]
            # If not then initialize ocean with default profile if `initial_file`
            # is "nothing", with `init_file` if it is nonempty.

            if read_restart

                println("`read_restart` is on. Look for rpointer file...")

                rpointer_file = joinpath(config["DRIVER"]["caserun"], config["MODEL_MISC"]["rpointer_file"])

                if !isfile(rpointer_file)
                    throw(ErrorException(format("File %s does not exist!", rpointer_file)))
                end
                
                writeLog(log_handle, "Reading rpointer file %s", rpointer_file)

                snapshot_filename = ""
                open(rpointer_file, "r") do file
                    snapshot_filename  = chomp(readline(file))
                end

                if !isfile(snapshot_filename)
                    throw(ErrorException(format("Initial file \"%s\" does not exist!", snapshot_filename)))
                end

                master_mb = EMOM.loadSnapshot(snapshot_filename; overwrite_config=core_config, log_handle=log_handle)
            else

                init_file = misc_config["init_file"]


                if init_file == "BLANK_PROFILE"

                    writeLog(log_handle, "`init_file` == '$(init_file)'. Initialize an empty ocean.")
                    master_ev = EMOM.Env(cfgs, verbose=is_master, log_handle=log_handle)
                    master_mb = EMOM.ModelBlock(master_ev; init_core=false)

                    master_mb.fi.sv[:TEMP] .= 20.0#rand(size(master_mb.fi.sv[:TEMP])...)
                    master_mb.fi.sv[:SALT] .= 35.0#rand(size(master_mb.fi.sv[:TEMP])...)

                elseif init_file != ""

                    println("Initial ocean with profile: ", init_file)
                    master_mb = EMOM.loadSnapshot(init_file; overwrite_config=core_config, log_handle=log_handle)
                
                else

                    throw(ErrorException("`init_file` cannot be empty string"))

                end
            end

            master_ev = master_mb.ev
        end

        if is_master
            if length(master_ev.cdata_varnames) != 0
                writeLog(log_handle, "The following datastream variables are used: %s.", join(master_ev.cdata_varnames, ", "))
            else
                writeLog(log_handle, "No datastream is used.")
            end
        end

                
        # It is necessary to use master_ev to create JobDistributionInfo
        master_ev_cfgs = nothing
        
        Nx = nothing
        Ny = nothing

        if is_master
            master_ev_cfgs = master_ev.cfgs
            Nx = master_ev.Nx
            Ny = master_ev.Ny
        end
        master_ev_cfgs = MPI.bcast(master_ev_cfgs, 0, comm)
        Nx = MPI.bcast(Nx, 0, comm)
        Ny = MPI.bcast(Ny, 0, comm)
            
        writeLog(log_handle, "Creating JobDistributionInfo...")
        jdi = JobDistributionInfo(Nx = Nx, Ny = Ny, overlap=3, comm=comm, master_rank=0)
        
        # Need to create Domain object accordingly
        domain = Parallelization.createDomain(jdi)
 
        # Second, create ModelBlocks based on ysplit_info
        if ! is_master
            master_mb = nothing
        end

        my_ev          = EMOM.Env(master_ev_cfgs; sub_yrng = getYsplitInfoByRank(jdi, rank).pull_fr_rng, log_handle=log_handle)
        my_mb          = EMOM.ModelBlock(my_ev; init_core = true)

        MPI.Barrier(comm)
        # Third, register all the variables.
        # Fourth, weaving MPI sending relation 

        x2o = nothing
        o2x = nothing

        empty_arr_sT = zeros(Float64, 1, my_ev.Nx, my_ev.Ny)
        x2o = Dict(
            "SWFLX"       => my_mb.fi.SWFLX,
            #"NSWFLX"      => my_mb.fi.NSWFLX,
            "TAUX_east"   => my_mb.fi.TAUX_east,
            "TAUY_north"  => my_mb.fi.TAUY_north,
            #"FRWFLX"      => copy(empty_arr_sT),
            #"VSFLX"       => my_mb.fi.VSFLX,
            "LWUP"    => my_mb.fi.LWUP,
            "LWDN"    => my_mb.fi.LWDN,
            "SEN"     => my_mb.fi.SEN,
            "LAT"     => my_mb.fi.LAT,
            "MELTH"   => my_mb.fi.MELTH,
            "MELTW"   => my_mb.fi.MELTW,
            "SNOW"    => my_mb.fi.SNOW,
            "IOFF"    => my_mb.fi.IOFF,
            "ROFF"    => my_mb.fi.ROFF,
            "PREC"    => my_mb.fi.PREC,
            "EVAP"    => my_mb.fi.EVAP,
            "SALTFLX" => my_mb.fi.SALTFLX,
        )

        o2x = Dict(
            "SST"         => my_mb.fi.sv[:SST],
            "Q_FRZMLTPOT" => my_mb.fi.Q_FRZMLTPOT,
            "USFC"        => my_mb.fi.USFC,
            "VSFC"        => my_mb.fi.VSFC,
            "mask"        => my_mb.ev.topo.sfcmask_sT,
        )

        # Synchronizing Data
        sync_data_list = Dict(

            # These forcings are syned from
            # master to slave before each
            # timestep starts.
            :forcing => (
                "SWFLX",
                "TAUX_east",
                "TAUY_north",
                "LWUP",
                "LWDN",
                "SEN",
                "LAT",
                "MELTH",
                "MELTW",
                "SNOW",
                "IOFF",
                "ROFF",
                "PREC",
                "EVAP",
                "SALTFLX",
            ),

            # These states will be synced from
            # slave to master right after
            # completing each timestep
            :output_state   => (
                "TEMP",
                "SALT",
                "UVEL",
                "VVEL",
                "WVEL",
                "CHKTEMP",
                "CHKSALT",
                "TAUX",
                "TAUY",
                "ADVT",
                "ADVS",
                "VDIFFT",
                "VDIFFS",
                "WKRSTT",
                "WKRSTS",
                "HMXL",
                "Q_FRZMLTPOT",
                "Q_FRZMLTPOT_NEG",
                "Q_FRZHEAT",
                "Q_FRZHEAT_OVERFLOW",
                "QFLXT",
                "QFLXS",
                "Ks_H_U",
                "Ks_H_V",
                "USFC",
                "VSFC",
                "NSWFLX",
                "VSFLX",
            ),

            # These states are synced from 
            # master to slave after :output_state
            # are syned
            :thermo_state   => (
                "TEMP",
                "SALT",
            ),

            :qflx_direct   => (
                "TEMP",
                "SALT",
                "HMXL",
            ),



            # These states are syned from
            # slave to master and master to slabe
            # during the substeps.
            :intm_state => (
                "INTMTEMP",
                "INTMSALT",
            )

        )
        
        sync_data_full = Dict()
        sync_data = Dict()
        for (k, l) in sync_data_list
            sync_data_full[k] = Array{DataUnit, 1}(undef, length(l))
            sync_data[k]      = Array{DataUnit, 1}(undef, length(l))
            for (n, varname) in enumerate(l)
                sync_data[k][n]      = my_mb.dt.data_units[varname]
                
                if is_master
                    sync_data_full[k][n] = master_mb.dt.data_units[varname]
                end
            end
        end

        MD = METADATA(
            casename,
            master_mb,
            my_mb,
            clock,
            x2o,
            o2x,
            config,
            nothing,
            jdi,
            sync_data_full,
            sync_data,
            comm,
            is_master,
            log_handle,
            domain,
        )



        if is_master

            activated_record = []
            MD.recorders = Dict()
            complete_variable_list = EMOM.getDynamicVariableList(master_mb; varsets=["ALL",])

#            additional_variable_list = EMOM.getVariableList(ocn, :COORDINATE)

            for rec_key in ["daily_record", "monthly_record"]
       
                activated = false
 
                println("# For record key: " * string(rec_key))

                varnames = Array{String}(undef, 0)
                varsets  = Array{String}(undef, 0)
                
                for v in misc_config[rec_key]
                    m = match(r"\{(?<varset>[a-zA-Z_]+)\}", v)
                    if m != nothing
                        push!(varsets, m[:varset])
                    else
                        push!(varnames, v)
                    end
                end
                    
                #println("varnames: ", varnames)
                rec_varnames = EMOM.getDynamicVariableList(master_mb; varnames=varnames, varsets=varsets) |> keys |> collect
                println("Record varnames: ", rec_varnames)

                add_varnames = EMOM.getCompleteVariableList(master_mb, :static) |> keys |> collect 

                #println("Additional varanames: ", add_varnames)

                #= 
                if typeof(misc_config[rec_key]) <: Symbol 
                    misc_config[rec_key] = EMOM.getVariableList(master_mb, misc_config[rec_key]) |> keys |> collect
                end

                # Qflux_finding mode requires certain output
                if my_ev.config[:Qflx_finding] == :on
                    append!(misc_config[rec_key], EMOM.getVariableList(master_mb, :QFLX_FINDING) |> keys )
                end
     
                # Load variables information as a list
                for varname in misc_config[rec_key]

                    println(format("Request output variable: %s", varname))
                    if haskey(complete_variable_list, varname)
                        println(format("Using varaible: %s", varname))
                        push!(var_list, varname)#, complete_variable_list[varname]... ) )
                    else
                        throw(ErrorException("Unknown varname in " * string(rec_key) * ": " * varname))
                    end
                end
                =#

                if length(rec_varnames) != 0
                    
                    MD.recorders[rec_key] = Recorder(
                        master_mb.dt,
                        rec_varnames,
                        EMOM.var_desc;
                        other_varnames=add_varnames,
                    )
                    

                    push!(activated_record, rec_key) 
                end
            end
 
            # Record (not output) happens AFTER the simulation.
            # Output of the current simulation happens at the
            # BEGINNING of the next simulation.
            #
            # Reason 1:
            # CESM does not simulate the first day of a `continue` run.
            # The first day has been simulated which is the last day of
            # the last run which is exactly the restart file. This is 
            # also why we have to call archive_record! function in the 
            # end of initialization.
            #
            # Reason 2:
            # Output happens at the beginning the next simulation. By
            # doing this we can get rid of the problem of deciding which
            # day is the end of month.
            #
            # This is also the way CAM chooses to do detect the end of
            # current month. 
            # See: http://www.cesm.ucar.edu/models/cesm1.0/cesm/cesmBbrowser/html_code/cam/time_manager.F90.html
            #      is_end_curr_month
            #

            # Must create the record file first because the
            # run of the first day is not called in CESM
            if misc_config["enable_archive"]

                function begOfNextMonth(t::AbstractCFDateTime)
                    y = Dates.year(t)
                    m = Dates.month(t)
                    return typeof(t)(y, m, 1) + Month(1)
                end

                function endOfNextMonth(t::AbstractCFDateTime)
                    y = Dates.year(t)
                    m = Dates.month(t)
                    return (typeof(t)(y, m, 1) + Month(2)) - Day(1)
                end

                if "daily_record" in activated_record
                    
                    recorder_day = MD.recorders["daily_record"]
                    mt = clock.cur_time
                    niters_per_day = Int(3600.0 / mt.cfg.dt)

                    addAlarm!(
                        clock,
                        "[Daily] Daily output",
                        addIters(mt, niters_per_day),
                        2;  
                        callback = function (clk, alm)
                            record!(recorder_day)
                            avgAndOutput!(recorder_day) # This is important
                        end,
                        recurring = niters_per_day,
                    )
                    println("My RANK: ", is_master, "; ", MPI.Comm_rank(comm))
                    addAlarm!(
                        clock,
                        "[Daily] Create daily output file.",
                        addIters(mt, 0), # Fire now so that first file can be created
                        1;
                        callback = function (clk, alm)
                            createRecordFile!(MD, "h1.day", recorder_day)
                        end,
                        recurring = niters_per_day*24,
                    )

                end
                #=
                if "monthly_record" in activated_record

                    # Design alarm such that
                    # (1) Create output file first
                    # (2) Record the initial condition
                    # (3) Record simulation after one day is stepped
                    # (4) If it is the first day of next month
                    #     (i)   avg and output data
                    #     (ii)  create next monthly file
                    #     (iii) record this step in the new file in (ii)
                    #     

                    recorder_mon = MD.recorders["monthly_record"]

                    addAlarm!(
                        clock,
                        "[Monthly] Daily accumulation using record!",
                        clock.time + Day(1),
                        3;
                        callback = function (clk, alm)
                            record!(recorder_mon)
                        end,
                        recurring = Day(1),
                    )

                    addAlarm!(
                        clock,
                        "[Monthly] Average and output monthly data.",
                        begOfNextMonth(clock.time), # Start from next month
                        2;  # Higher priority so it outputs data before creating next new monthly file
                        callback = function (clk, alm)
                            avgAndOutput!(recorder_mon)
                        end,
                        recurring = begOfNextMonth,
                    )

                    addAlarm!(
                        clock,
                        "[Monthly] Create monthly output file.",
                        clock.time, # Rings immediately
                        1;
                        callback = function (clk, alm)
                            createRecordFile!(MD, "h0.mon", recorder_mon)
                        end,
                        recurring = begOfNextMonth,
                    )


     
                end
                =#
            end
            
        end

        MPI.Barrier(comm)

        # Test. Should be removed later
        if is_master
            for j = 1:Ny
                MD.mb_full.fi.sv[:TEMP][:, :, j] .= j
            end
        end

        syncField!(
            MD.sync_data[:thermo_state],
            MD.jdi,
            :M2S,
            :BLOCK;
            vars_master = MD.sync_data_full[:thermo_state],
        ) 
      
 
        # Setup HMXL depth
        if MD.config["MODEL_CORE"]["MLD_scheme"] == "SOM"
            MD.mb.fi.HMXL .= - MD.mb.ev.topo.topoz_sT
            if any(MD.mb.fi.HMXL .< 0)
                throw(ErrorException("Some HMXL is negative. Please check."))
            end
        end

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
        is_master = MD.is_master

        Δt_float = Float64(MD.clock.time_cfg.dt * niters)
        
        substeps = MD.mb.ev.cfgs["MODEL_CORE"]["substeps"]
        Δt_substep = Δt_float / substeps
        
        syncField!(
            MD.sync_data[:forcing],
            MD.jdi,
            :M2S,
            :BLOCK;
            vars_master = MD.sync_data_full[:forcing],
        )

        EMOM.reset!(MD.mb.co.wksp)
        EMOM.updateSfcFlx!(MD.mb)
        EMOM.updateDatastream!(MD.mb, MD.clock)
        EMOM.updateUVSFC!(MD.mb)
        EMOM.checkBudget!(MD.mb, Δt_float; stage=:BEFORE_STEPPING)

        Δz_min = minimum(view(MD.mb.ev.gd.Δz_T, :, 1, 1))
        EMOM.setupForcing!(MD.mb; w_max = Δz_min / Δt_substep * 0.9)

        # Because substep in stepAdvection updates _INTMX_
        # based on _INTMX_ itself, we need to copy it first.
        MD.mb.tmpfi._INTMX_ .= MD.mb.fi._X_
        
        for substep = 1:substeps

            EMOM.reset!(MD.mb.co.wksp)
            EMOM.stepAdvection!(MD.mb, Δt_substep)
            EMOM.checkBudget!(MD.mb, Δt_float; stage=:SUBSTEP_AFTER_ADV, substeps=substeps)

            syncField!(
                MD.sync_data[:intm_state],
                MD.jdi,
                :S2M,
                :BND;
                vars_master = MD.sync_data_full[:intm_state],
            )

            syncField!(
                MD.sync_data[:intm_state],
                MD.jdi,
                :M2S,
                :BND;
                vars_master = MD.sync_data_full[:intm_state],
            )
        end

        EMOM.stepColumn!(MD.mb, Δt_float)
        EMOM.checkBudget!(MD.mb, Δt_float; stage=:AFTER_STEPPING)

        # important: update X
        MD.mb.fi._X_ .= MD.mb.tmpfi._NEWX_
       
        syncField!(
            MD.sync_data[:output_state],
            MD.jdi,
            :S2M,
            :BLOCK;
            vars_master = MD.sync_data_full[:output_state],
        )
        
        syncField!(
            MD.sync_data[:thermo_state],
            MD.jdi,
            :M2S,
            :BND;
            vars_master = MD.sync_data_full[:thermo_state],
        ) 

        if write_restart
            writeLog("`wrtie_restart` is true.")
            if is_master 
                writeRestart(MD)
            end
        end


 
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

    function createRecordFile!(
        MD     :: METADATA,
        group  :: String,
        recorder :: Recorder,
    )

        iter = MD.clock.cur_time.iter

        filename = @sprintf("%s.EMOM.%s.%06d.nc", MD.casename, group, iter)

        setNewNCFile!(
            recorder,
            joinpath(MD.config["DRIVER"]["caserun"], filename)
        )
            
        appendLine(joinpath(MD.config["DRIVER"]["caserun"], MD.config["DRIVER"]["archive_list"]), 
            @sprintf("mv,%s,%s,%s",
                filename,
                MD.config["DRIVER"]["caserun"],
                joinpath(MD.config["DRIVER"]["archive_root"], "ocn", "hist"),
            )
        )

    end


    function writeRestart(
        MD :: METADATA,
    )

        mt = MD.clock.cur_time

        timestamp_str = @sprintf(
            "%10d",
            mt.iter,
            #Dates.format(clock_time, "yyyy-mm-dd"),
            #floor(Int64, Dates.hour(clock_time)*3600+Dates.minute(clock_time)*60+Dates.second(clock_time)),
        )

        snapshot_filename = @sprintf(
            "%s.snapshot.%s.jld2",
            MD.config["DRIVER"]["casename"],
            timestamp_str,
        )


        EMOM.takeSnapshot(
            MD.clock.time,
            MD.mb,
            joinpath(
                MD.config["DRIVER"]["caserun"],
                snapshot_filename,
            ),
        )

        println("(Over)write restart pointer file: ", MD.config["MODEL_MISC"]["rpointer_file"])
        open(joinpath(MD.config["DRIVER"]["caserun"], MD.config["MODEL_MISC"]["rpointer_file"]), "w") do io
            write(io, snapshot_filename, "\n")
        end

        appendLine(joinpath(MD.config["DRIVER"]["caserun"], MD.config["DRIVER"]["archive_list"]), 
            @sprintf("cp,%s,%s,%s",
                snapshot_filename,
                MD.config["DRIVER"]["caserun"],
                joinpath(MD.config["DRIVER"]["archive_root"], "rest", timestamp_str),
            )
        )
        
        appendLine(joinpath(MD.config["DRIVER"]["caserun"], MD.config["DRIVER"]["archive_list"]), 
            @sprintf("cp,%s,%s,%s",
                MD.config["MODEL_MISC"]["rpointer_file"],
                MD.config["DRIVER"]["caserun"],
                joinpath(MD.config["DRIVER"]["archive_root"], "rest", timestamp_str),
            )
        )
        
    end



    #=
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
    =# 
end
