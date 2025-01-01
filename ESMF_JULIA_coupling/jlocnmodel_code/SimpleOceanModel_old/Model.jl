mutable struct Model

    env   :: Env
    state :: State 
    core  :: Core

    
    function Model(
        env :: Env;
    )
    
        println("You are in the OceanModel constructor (ta-da)!")
        
        state = State(env)
        core  = Core(env)

        o = new(
            env,
            state,
            core,
        )

        return o

    end



end

