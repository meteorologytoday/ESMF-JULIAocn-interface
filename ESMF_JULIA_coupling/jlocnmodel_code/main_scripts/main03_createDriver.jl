
# Still need to think about what is the advandage
# of separating the initilization of an interface.
# I.e. why can't I do createInterface ?

cpl_funcs = CouplingModule.CouplerFunctions()

cpl_funcs.master_before_model_init = function(
    msg :: Dict,
)

    writeLog("[Coupler] Before model init. My rank = %d", rank)
    
    if msg["MSG"] != "INIT"
        throw(ErrorException("Unexpected `MSG` : " * string(msg["MSG"])))
    end

    read_restart = msg["READ_RESTART"]
    cpl_time = parseCESMTIME(msg["CESMTIME"], dr.timetype)
    Δt = Dates.Second(parse(Float64, msg["DT"]))

    return read_restart, cesm_coupler_time, Δt

end



@printf("[DRIVER] Create an empty interface\n")
cpl_if = CouplingModule.CouplingInterface(
    config_file,
    CouplingModule.createEmptyCouplerFunctions(),#SimpleOceanModel.createCouplerFunctions()
)

rank = MPI.Comm_rank(COMM)

@printf("[DRIVER] Creating a log handle... \n")


@printf("[DRIVER] Create a DRIVER... \n")
is_master = rank == 0
log_handle = createLogHandle(MPI.Comm_rank(COMM), true)
dr = DriverModule.Driver(
    config,
    OMMODULE, # Defined in main01_loadModule_[MODEL_NAME].jl
    COMM,
    is_master,
    cpl_if, 
    log_handle,
)

@printf("[DRIVER] Finished Running File %s\n", @__FILE__)

