module SimpleOceanModel_CORE
    
    using Printf
    using ..LogSystem

    macro hinclude(path)
        return :(include(normpath(joinpath(@__DIR__, $path))))
    end
   
    @hinclude("Env.jl")
    @hinclude("TempField.jl")
    @hinclude("Field.jl")
    @hinclude("Toolbox.jl")
    @hinclude("Tile.jl")
    
end



