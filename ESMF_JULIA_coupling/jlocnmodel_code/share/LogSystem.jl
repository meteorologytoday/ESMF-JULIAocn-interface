module LogSystem

    using Printf
    using MPI
    
    export writeLog
    
    function writeLog(args...; force :: Bool = false)
        
        comm = MPI.COMM_WORLD
        rank = MPI.Comm_rank(comm)

        if force || rank == 0
            
            s = Printf.format(args...)
            @printf("%s\n", s)
        end
    end
end
