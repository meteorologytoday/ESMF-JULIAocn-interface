
module OceanModel

    using MPI
    using TOML

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


    function getDomainInfo(m :: Model)

        result = nothing
        gd = m.env.gd

        try
            # For now we only support nSx = nSy = 1 
            myXGlobalLo = gd.myXGlobalLo[1]
            myYGlobalLo = gd.myYGlobalLo[1]
            println("Ready to send domain info...")
            
            result = (
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
            )
 
        catch e

            println("Exception occurs: ", e) 
            throw(e)
          
        end

        println("Gonna return : ", result)
    
        return result
    end
end



       
