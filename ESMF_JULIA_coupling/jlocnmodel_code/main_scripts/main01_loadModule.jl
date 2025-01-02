
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


config_file = "config_SimpleOceanModel.toml"

@printf("[DRIVER] Create an empty interface\n")
cpl_if = CouplingModule.CouplingInterface(
    config_file,
    CouplingModule.createEmptyCouplerFunctions(),#SimpleOceanModel.createCouplerFunctions()
)
        

@printf("[DRIVER] Creating a log handle... ")
log_handle = createLogHandle(MPI.Comm_rank(COMM))

@printf("[DRIVER] Obtain config from: %s\n", config_file)
config = TOML.parsefile(config_file)

dr = DriverModule.Driver(
    config,
    SimpleOceanModel,
    COMM,
    cpl_if, 
    log_handle,
)

