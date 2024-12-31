using Printf
using TOML
@printf("Running %s\n", @__FILE__)


@printf("This program emulate what ESMF is doing, serving as a debugging tool.\n")
@printf("This program also has the potential to serve as a future prototype of Julia centered coupler.\n")


include("MPITools/MPI_essentials.jl")
include("Interface/ControlInterface.jl")
include("SingleColumnOceanModel/SingleColumnOceanModel.jl")
include("Driver.jl")

using .ControlInterface
using .SingleColumnOceanModel
using .Driver


config_file = "config_SingleColumnOceanModel.toml"

@printf("Initialize MPI.\n")
MPI.Init()

@printf("Pretend passing an MPI communicator to ocean model driver.\n")
passMPICommunicator(MPI.COMM_WORLD)

@printf("My rank is %d of size %d. Am I the master? %s\n", RANK, MPI.Comm_size(COMM), string(IS_MASTER))


# Still need to think about what is the advandage
# of separating the initilization of an interface.
# I.e. why can't I do createInterface ?
@printf("Create an empty interface\n")
cpl_funcs = SingleColumnOceanModel.createCouplerFunctions()

@printf("Setup an interface\n")
interface = ControlInterface.Interface(
    config_file,
)

OMMODULE = SingleColumnOceanModel


@printf("Obtain config from: %s\n", config_file)
config = TOML.parsefile(config_file)
Driver.runModel(
    OMMODULE,
    COMM,
    config,
)

@printf("End of file %s\n", @__FILE__)




