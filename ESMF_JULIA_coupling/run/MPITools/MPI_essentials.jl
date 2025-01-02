using MPIPreferences
MPIPreferences.use_system_binary(; library_names=["/home/t2hsu/miniconda3/envs/mpi/lib/libmpi"])

using MPI

function passMPICommunicator(
    #recv_comm :: Union{MPI.Comm, Vector},
    recv_comm :: Any,
)
    #println("Here in passMPICommunicator")

    #println("comm_array = ", recv_comm, "; type = ", typeof(recv_comm))
    if typeof(recv_comm) == MPI.Comm
        comm = recv_comm
    else
        comm = MPI.Comm(recv_comm)
    end
    
    println("Rank: $(MPI.Comm_rank(comm))/$(MPI.Comm_size(comm)) ; COMM_WORLD Rank: $(MPI.Comm_rank(MPI.COMM_WORLD))/$(MPI.Comm_size(MPI.COMM_WORLD))")

    @printf("Creating global variabls: COMM, RANK, IS_MASTER\n")
    global COMM = comm
    global RANK = MPI.Comm_rank(COMM)
    global IS_MASTER = RANK == 0
    
    #println("Exiting passMPICommunicator")
end

