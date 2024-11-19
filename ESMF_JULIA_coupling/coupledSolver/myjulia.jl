
using MPI

function add_numbers(thread_id::Int32, comm::Int32)

    
    try

        if ! MPI.Initialized()
            println("[JL] MPI is not initialized.")
        else
            println("[JL] MPI is initialized.")
        end
        
        println("Thread id = ", thread_id)
        println("comm = ", comm )
        println("Getting MPI communicator handle with number = ", comm)
        comm_handle = MPI.Comm(comm)

        println("Comm_handle = ", comm_handle)

        println("Hello world, I am rank $(MPI.Comm_rank(comm_handle))")


        #=
        println("Looks like I get it!")
        println("The handle got: ", comm_handle)

       
        comm_handle = MPI.COMM_WORLD
 
        println("Hello world, I am rank $(MPI.Comm_rank(comm_handle))")
        println("Hello world again, there are in total $(MPI.Comm_size(comm_handle)) threads.")
        =#

    catch e

        println("An error occurred: $e")
        
        # Print the stack trace
        println("Stack trace:")
        for frame in stacktrace(e)
            println(frame)
        end
    end

    #MPI.Barrier(comm)
    
end

module myJuliaModule

    function divide_numbers(a, b)

        println("a / b = ", a/b)
        return a/b

    end


end
