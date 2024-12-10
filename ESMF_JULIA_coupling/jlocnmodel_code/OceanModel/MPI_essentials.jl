using MPIPreferences
MPIPreferences.use_system_binary(; library_names=["/home/t2hsu/miniconda3/envs/mpi/lib/libmpi"])

using MPI

function passMPICommunicator(
    comm_array :: Union{MPI.Comm, Vector},
)
    println("Here in passMPICommunicator")
    println("Comm_array = ", comm_array)
    println(typeof(comm_array))
    if typeof(comm_array) == MPI.Comm
        comm = comm_array
    else
        comm = MPI.Comm(comm_array[1])
    end
    println(comm)
    println("Rank: $(MPI.Comm_rank(comm)) of $(MPI.Comm_size(comm))")

    @printf("Creating global variabls: COMM, RANK, IS_MASTER\n")
    global COMM = comm
    global RANK = MPI.Comm_rank(COMM)
    global IS_MASTER = RANK == 0

    println("Exiting passMPICommunicator")
end

