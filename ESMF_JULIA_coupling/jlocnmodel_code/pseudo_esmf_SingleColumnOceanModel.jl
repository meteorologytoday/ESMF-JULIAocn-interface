using Printf
@printf("Running %s\n", @__FILE__)
include("MPITools/MPI_essentials.jl")
include("Interface/ControlInterface.jl")

@printf("Initialize MPI.\n")
MPI.Init()

@printf("Pretend passing an MPI communicator to ocean model driver.\n")
passMPICommunicator(MPI.COMM_WORLD)

@printf("My rank is %d of size %d. Am I the master? %s\n", RANK, MPI.Comm_size(COMM), string(IS_MASTER))


@printf("Create an empty interface\n")
cpl_funcs = SingleColumnOceanModel.createCouplerFunctions()

@printf("Setup an interface\n")
interface = ControlInterface.Interface(
    config_file,
)

@printf("Run model?\n")


@printf("End of file %s\n", @__FILE__)




