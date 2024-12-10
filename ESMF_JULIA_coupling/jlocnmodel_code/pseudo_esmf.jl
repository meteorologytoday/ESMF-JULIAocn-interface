using Printf
@printf("Running %s\n", @__FILE__)


include("OceanModel/MPI_essentials.jl")
include("OceanModel/ControlInterface.jl")




@printf("Initialize MPI.\n")
MPI.Init()

@printf("Pretend passing an MPI communicator to ocean model driver.\n")
passMPICommunicator(MPI.COMM_WORLD)

@printf("My rank is %d of size %d. Am I the master? %s\n", RANK, MPI.Comm_size(COMM), string(IS_MASTER))



