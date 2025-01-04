
root = joinpath("..", "..")

include(joinpath(root, "main_scripts/main01_loadModule_EkmanMixedlayerOceanModel.jl"))
include(joinpath(root, "main_scripts/main02_loadMiscModule.jl"))

# Here is the part 
@printf("Initialize MPI.\n")
MPI.Init()

@printf("Pretend passing an MPI communicator to ocean model driver.\n")
passMPICommunicator(MPI.COMM_WORLD)



include(joinpath(root, "main_scripts/main03_createDriver.jl"))
include(joinpath(root, "main_scripts/main04_init.jl"))

domain_params = zeros(Int64, 13)
DriverModule.getDomainInfo(dr, domain_params)
println("domain_params = ", domain_params)

cnt = 0
while ! ifClockEnds(dr.clock)
    writeLog(log_handle, "[PSEUDO_ESMF] (cnt=%d) Timestep model.", cnt)
    include(joinpath(root, "main_scripts/main05_run.jl"))
    global cnt += 1
end

include(joinpath(root, "main_scripts/main06_finalize.jl"))

@printf("End of file %s\n", @__FILE__)




