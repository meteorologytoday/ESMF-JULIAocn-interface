using MPI
println("Initialize MPI")
MPI.Init()


function add_numbers(thread_id, comm)

    println("Thread id = ", thread_id)
    println("comm = ", comm )
    

    println("Getting MPI communicator handle with number = ", comm)
    #comm_handle = MPI.Comm(comm)
    comm_handle = MPI.COMM_WORLD#Comm(comm)

    println("Looks like I get it!")
    
    println(comm_handle)
    println("Hello world, I am rank $(MPI.Comm_rank(comm_handle))")
    println("Hello world again, there are in total $(MPI.Comm_size(comm_handle)) threads.")

    #MPI.Barrier(comm)

end

module myJuliaModule

    function divide_numbers(a, b)

        println("a / b = ", a/b)
        return a/b

    end


end
