using MPIPreferences
MPIPreferences.use_system_binary(; library_names=["/home/t2hsu/miniconda3/envs/mpi/lib/libmpi"])

using MPI



function passMPICommunicator(comm_array)

    println("Here in passMPICommunicator")

    println("Comm_array = ", comm_array)
    comm = MPI.Comm(comm_array[1])
    println(comm)
    println("Rank: $(MPI.Comm_rank(comm)) of $(MPI.Comm_size(comm))")

    global COMM_ROOT = comm

    println("Exiting passMPICommunicator")
end

