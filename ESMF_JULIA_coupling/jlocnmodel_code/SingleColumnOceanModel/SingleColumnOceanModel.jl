include( normpath(joinpath("..", "Interface", "ControlInterface.jl")) )

module SingleColumnOceanModel

    using MPI
    using TOML
    using Printf
    using JSON

    include("Grid.jl")
    include("Env.jl")
    include("State.jl")
    include("Core.jl")
    include("Model.jl")

    function createOceanModel(
        config_file :: String;
        comm :: Union{MPI.Comm, Nothing} = nothing,
    )

        model = nothing

        try
            println("Initiating Env...")
            
            env = OceanModel.Env(; 
                model_config_file = config_file,
                comm = comm,
            )

            model = OceanModel.Model(env)
            println("Report ocean model...")
            OceanModel.report(model)

        catch e

            println("Exception occurs: ", e) 
            throw(e)
            
        end

        if model == nothing
            println("Model is `nothing`. There must be some error during model creation. Please check.")
        end
        
        return model
    end

    function report(m :: Model)

        comm = m.env.comm
        println("MPI information:")
        println(" - MPI : ", comm)
        println(" - MPI Size: ", MPI.Comm_size(comm))
        println(" - MPI Rank: ", MPI.Comm_rank(comm))
        
        println("End of Report")
    end


    function getDomainInfo!(
        m :: Model,
        params :: Vector{Int64},
    )

        gd = m.env.gd

        try
            # For now we only support nSx = nSy = 1 
            myXGlobalLo = gd.myXGlobalLo[1]
            myYGlobalLo = gd.myYGlobalLo[1]
            println("Ready to send domain info...")
            
            
            for (i, value) in enumerate((
                gd.sNx,
                gd.sNy,
                gd.OLx,
                gd.OLy,
                gd.nSx,
                gd.nSy,
                gd.nPx,
                gd.nPy,
                gd.Nx,
                gd.Ny,
                gd.Nr,
                myXGlobalLo,
                myYGlobalLo,
            ))
                params[i] = value
            end
 
        catch e

            println("Exception occurs: ", e) 
            throw(e)
          
        end

    end

    function receiveMessage!(
        model :: Model,
        msg   :: String,
    )
        
        @printf("I receive this message: %s\n", msg)

        try
            @printf("Parsed as JSON: \n")
            parsed = JSON.parse(msg)
            JSON.print(parsed, 4)
        catch e
            println("Exception occurs: ", e) 
            throw(e)
        end


    end
end



function createCouplerFunctions()

    cpl_funcs = CouplerFunctions()

    cpl_funcs.master_before_model_init = function()

        #=  
        writeLog("[Coupler] Before model init. My rank = {:d}", rank)
        
        recvMsg()
       
        if msg["MSG"] != "INIT"
            throw(ErrorException("Unexpected `MSG` : " * string(msg["MSG"])))
        end
 
        read_restart = (msg["READ_RESTART"] == "TRUE") ? true : false
        cesm_coupler_time = parseCESMTIME(msg["CESMTIME"], timetype)
        Δt = Dates.Second(parse(Float64, msg["DT"]))
        =#

        return read_restart, cesm_coupler_time, Δt
        
    end

    cpl_funcs.master_after_model_init! = function(OMMODULE, OMDATA)

            #global msg

            #=
            writeLog("[Coupler] After model init. My rank = {:d}", rank)

            global lsize = parse(Int64, msg["LSIZE"])

            global send_data_list = [OMDATA.o2x["SST"], OMDATA.o2x["Q_FRZMLTPOT"], OMDATA.o2x["USFC"], OMDATA.o2x["VSFC"]]
            global recv_data_list = []

            global x2o_available_varnames = split(msg["VAR2D"], ",")
            global x2o_wanted_varnames = keys(OMDATA.x2o)
            global x2o_wanted_flag     = [(x2o_available_varnames[i] in x2o_wanted_varnames) for i = 1:length(x2o_available_varnames)]


            writeLog("# Check if all the requested variables are provided by cesm end...")
            local pass = true
            for varname in x2o_wanted_varnames
                if ! ( varname in x2o_available_varnames )
                    writeLog("Error: $(varname) is requested by ocean model but not provided on cesm side.")
                    pass = false
                end 
            end

            if pass
                writeLog("All variables requested are provided.")
            else
                throw(ErrorException("Some variable are not provided. Please check."))
            end

            writeLog("# List of provided x2o variables:")
            for (i, varname) in enumerate(x2o_available_varnames)
                push!(recv_data_list , ( x2o_wanted_flag[i] ) ? OMDATA.x2o[varname] :  zeros(Float64, lsize))
                println(format(" ({:d}) {:s} => {:s}", i, varname, ( x2o_wanted_flag[i] ) ? "Wanted" : "Dropped" ))
            end


            sendData(PTI, "OK", send_data_list)
            sendData(PTI, "mask",  [OMDATA.o2x["mask"],])
            =#


    end

    cpl_funcs.master_before_model_run! = function(OMMODULE, OMDATA)
        #writeLog("[Coupler] Before model run")
        #writeLog("[Coupler] This is where flux exchange happens.")

        #=
        recvMsg() 

        return_values = nothing
        if msg["MSG"] == "RUN"
            Δt = Dates.Second(parse(Float64, msg["DT"]))
            recvData!(
                PTI,
                recv_data_list,
                which=2,
            )
            
            cesm_coupler_time = parseCESMTIME(msg["CESMTIME"], timetype)
            if OMDATA.clock.time != cesm_coupler_time
                writeLog("Warning: time inconsistent. `cesm_coupler_time` is $(string(cesm_coupler_time)), but ocean model's time is $(string(OMDATA.clock.time)). This can happen if this is a startup run or hybrid run. Because my implementation cannot tell, I have to just forward the model time. Please be extra cautious about this.")
           
                error_t = Second(cesm_coupler_time - OMDATA.clock.time)
                if error_t.value < 0
                    throw(ErrorException("Error: ocean model time is ahead of `cesm_coupler_time`. This is absolutely an error."))
                end
                advanceClock!(OMDATA.clock, error_t)
                
            end
 
            write_restart = msg["WRITE_RESTART"] == "TRUE"

            return_values = ( :RUN,  Δt, write_restart )

        elseif msg["MSG"] == "END"
            return_values = ( :END, 0.0, false  )
        else
            throw(ErrorException("Unexpected `MSG` : " * string(msg["MSG"])))
        end
        =#

        return return_values

    end

    cpl_funcs.master_after_model_run! = function(OMMODULE, OMDATA)
        #=
        #writeLog("[Coupler] After model run")
        global send_data_list = [OMDATA.o2x["SST"], OMDATA.o2x["Q_FRZMLTPOT"], OMDATA.o2x["USFC"], OMDATA.o2x["VSFC"]]
        sendData(PTI, "OK", send_data_list)
        =#

    end

    cpl_funcs.master_finalize! = function(OMMODULE, OMDATA)
        #=

        writeLog("[Coupler] Finalize")
        writeLog("Sleep for 30 seconds before archiving to avoid conflicting with CESM archiving process.")
        sleep(30.0)
        =#

    end 

    return cpl_funcs
end


       
