
mutable struct Env
    
    comm  :: MPI.Comm
    model_config_file :: String
    model_config :: Dict
    gd :: Grid
    
    function Env(;
        model_config_file :: String ="model_config.toml",
        comm :: Union{MPI.Comm, Nothing} = nothing,
    )

        if comm == nothing
            println("Communicator not received. Using default : MPI.COMM_WORLD")
            comm = MPI.COMM_WORLD
        end

        println("Parsing file: ", model_config_file)
        model_config = TOML.parsefile(model_config_file)
        
        gd = Grid(
            model_config;
            pet_size = MPI.Comm_size(comm),
            pet_rank = MPI.Comm_rank(comm),
        )

        return new(
            comm,
            model_config_file,
            model_config,
            gd,
        )

    end
end

