module ModelTimeModule

    export ModelTime, string, collapseTime

    using Printf
    
    mutable struct ModelTime
        # time = ref + dt * n
        ref :: Float64 # The reference where time
        dt  :: Rational{Int64}
        n   :: Int64   # Iteration   
    end

    function string(mt :: ModelTime)
        return @sprintf(
            "(ref=%f, dt=%s, n=%d)",
            mt.ref,
            Base.string(mt.dt),
            mt.n,
        )
    end 

    function collapseTime(mt :: ModelTime)
        return mt.ref + Float64(mt.dt * mt.n)
    end 
    


end

function Base.:(>)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
    println(">")
    return ModelTimeModule.collapseTime(a) > ModelTimeModule.collapseTime(b)
end

function Base.:(<)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
    println("<")
    return ModelTimeModule.collapseTime(a) < ModelTimeModule.collapseTime(b)
end

function Base.:(==)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
    println("==")
    return ModelTimeModule.collapseTime(a) == ModelTimeModule.collapseTime(b)
end


