include("main_scripts/main00_loadModule.jl")

# Here is the part 
@printf("Initialize MPI.\n")
MPI.Init()

@printf("Pretend passing an MPI communicator to ocean model driver.\n")
passMPICommunicator(MPI.COMM_WORLD)


include("main_scripts/main01_loadModule.jl")
include("main_scripts/main02_init.jl")

domain_params = zeros(Int64, 13)
DriverModule.getDomainInfo(dr, domain_params)
println("domain_params = ", domain_params)


include("main_scripts/main03_run.jl")
include("main_scripts/main04_finalize.jl")

@printf("End of file %s\n", @__FILE__)




