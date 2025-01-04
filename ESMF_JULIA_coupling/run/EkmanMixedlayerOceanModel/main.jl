using Printf
@printf("Running %s\n", @__FILE__)

using MPI
using JSON
using TOML

include("MPITools/MPI_essentials.jl")
include("Interface/ControlInterface.jl")
include("EMOM/ENGINE_EMOM.jl")
include("Driver.jl")

config_file = "config_SingleColumnOceanModel.toml"

@printf("Initialize MPI.\n")
MPI.Init()

@printf("Pretend passing an MPI communicator to ocean model driver.\n")
passMPICommunicator(MPI.COMM_WORLD)

@printf("My rank is %d of size %d. Am I the master? %s\n", RANK, MPI.Comm_size(COMM), string(IS_MASTER))


comm = MPI.COMM_WORLD
rank = MPI.Comm_rank(comm)
is_master = rank == 0

coupler_funcs = (

    master_after_model_init! = function(dr :: Driver)

        global msg

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

    end,

    master_before_model_run! = function(OMMODULE, OMDATA)
        #writeLog("[Coupler] Before model run")
        #writeLog("[Coupler] This is where flux exchange happens.")
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

        return return_values

    end,

    master_after_model_run! = function(OMMODULE, OMDATA)
        #writeLog("[Coupler] After model run")
        global send_data_list = [OMDATA.o2x["SST"], OMDATA.o2x["Q_FRZMLTPOT"], OMDATA.o2x["USFC"], OMDATA.o2x["VSFC"]]
        sendData(PTI, "OK", send_data_list)
    end,

    master_finalize! = function(OMMODULE, OMDATA)
        writeLog("[Coupler] Finalize")
        writeLog("Sleep for 30 seconds before archiving to avoid conflicting with CESM archiving process.")
        sleep(30.0)
    end 
)



runModel(
    ENGINE_EMOM, 
    coupler_funcs,
    config, 
)
