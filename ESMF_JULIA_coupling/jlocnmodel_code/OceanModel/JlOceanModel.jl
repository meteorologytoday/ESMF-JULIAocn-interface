
module OceanModel

    using MPI
    using TOML
    using Printf
    using JSON

    include("Grid.jl")
    include("Env.jl")
    include("State.jl")
    include("Core.jl")
    include("Model.jl")

    function createOceanModel(
        config_file :: String;
        comm :: Union{MPI.Comm, Nothing} = nothing,
    )

        model = nothing

        try
            println("Initiating Env...")
            
            env = OceanModel.Env(; 
                model_config_file = config_file,
                comm = comm,
            )

            model = OceanModel.Model(env)
            println("Report ocean model...")
            OceanModel.report(model)

        catch e

            println("Exception occurs: ", e) 
            throw(e)
    
        end

        if model == nothing
            println("Model is `nothing`. There must be some error during model creation. Please check.")
        end
        
        return model
    end

    function report(m :: Model)

        comm = m.env.comm
        println("MPI information:")
        println(" - MPI : ", comm)
        println(" - MPI Size: ", MPI.Comm_size(comm))
        println(" - MPI Rank: ", MPI.Comm_rank(comm))
        
        println("End of Report")
    end


    function getDomainInfo!(
        m :: Model,
        params :: Vector{Int64},
    )

        gd = m.env.gd

        try
            # For now we only support nSx = nSy = 1 
            myXGlobalLo = gd.myXGlobalLo[1]
            myYGlobalLo = gd.myYGlobalLo[1]
            println("Ready to send domain info...")
            
            
            for (i, value) in enumerate((
                gd.sNx,
                gd.sNy,
                gd.OLx,
                gd.OLy,
                gd.nSx,
                gd.nSy,
                gd.nPx,
                gd.nPy,
                gd.Nx,
                gd.Ny,
                gd.Nr,
                myXGlobalLo,
                myYGlobalLo,
            ))
                params[i] = value
            end
 
        catch e

            println("Exception occurs: ", e) 
            throw(e)
          
        end

    end

    function receiveMessage!(
        model :: Model,
        msg   :: String,
    )
        
        @printf("I receive this message: %s\n", msg)

        try
            @printf("Parsed as JSON: \n")
            parsed = JSON.parse(msg)
            JSON.print(parsed, 4)
        catch e
            println("Exception occurs: ", e) 
            throw(e)
        end


    end
end



       
