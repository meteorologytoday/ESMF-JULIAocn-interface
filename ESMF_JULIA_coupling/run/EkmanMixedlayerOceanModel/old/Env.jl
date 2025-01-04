mutable struct Env

    cfgs :: Dict
    
    function Env(
        cfgs;
        verbose :: Bool = false,
    )
        return new(
            cfgs,
        )
    end

end


