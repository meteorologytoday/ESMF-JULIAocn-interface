module ModelTimeModule

    export ModelTime, string, collapseTime
    export setIteration!
    export copy_partial, copy_full, string

    using Printf
 
    mutable struct ModelTimeConfig
        # time = ref + dt * iter
        ref  :: Float64 # The reference where time
        dt   :: Rational{Int64}
    end

   
    # abstime = ref + dt * iter
    mutable struct ModelTime
        cfg :: ModelTimeConfig
        iter :: Int64   # Iteration   
    end

    function copy_full(mt :: ModelTime)
        cfg = ModelTimeConfig(mt.cfg.ref, copy(mt.cfg.dt))
        new_mt = ModelTime(cfg, mt.iter)
        return new_mt
    end 

    function copy_partial(mt :: ModelTime)
        new_mt = ModelTime(mt.cfg, mt.iter)
        return new_mt
    end 

    function string(mt :: ModelTime)
        return @sprintf(
            "(ref=%f, dt=%s, iter=%d)",
            mt.cfg.ref,
            Base.string(mt.cfg.dt),
            mt.iter,
        )
    end 

    function collapseTime(mt :: ModelTime)
        return mt.cfg.ref + Float64(mt.cfg.dt * mt.iter)
    end 

    function isConsistent(a :: ModelTime, b::ModelTime)
        
        if a.cfg === b.cfg || ( a.cfg.ref === b.cfg.ref && a.cfg.dt == b.cfg.dt )
            return true
        else
            return false
        end
        
    end
end

#Base.string(mt :: ModelTimeModule.ModelTime) = ModelTimeModule.string(mt)
#Base.copy(mt :: ModelTimeModule.ModelTime) = ModelTimeModule.copy(mt)


function Base.:(>)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
    if isConsistent(a, b)
        return a.iter > b.iter
    else
        return ModelTimeModule.collapseTime(a) > ModelTimeModule.collapseTime(b)
    end
end

function Base.:(<)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
    if isConsistent(a, b)
        return a.iter < b.iter
    else
        return ModelTimeModule.collapseTime(a) < ModelTimeModule.collapseTime(b)
    end
end

function Base.:(==)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
    if isConsistent(a, b)
        return a.iter == b.iter
    else
        return ModelTimeModule.collapseTime(a) == ModelTimeModule.collapseTime(b)
    end
end


