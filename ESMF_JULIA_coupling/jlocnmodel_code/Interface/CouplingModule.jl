module CouplingModule

    using TOML

    mutable struct CouplerFunctions

        master_before_model_init :: Union{Function, Nothing}
        master_after_model_init! :: Union{Function, Nothing}
        master_before_model_run! :: Union{Function, Nothing}
        master_after_model_run!  :: Union{Function, Nothing}
        master_finalize!         :: Union{Function, Nothing}

        function CouplerFunctions()
            cpl_funs = new([nothing for i=1:5]...)
            return cpl_funs
        end
    end

    struct CouplingInterface
        
        config      :: Dict
        cpl_funs    :: Union{CouplerFunctions, Nothing}
        
        function CouplingInterface(
            config :: Union{String, Dict},
            cpl_funs :: Union{Nothing, CouplerFunctions} = nothing,
        )

            if typeof(config) <: String
                config = TOML.parsefile(config)
            end
            
            return new(
                config,
                cpl_funs,
            )
        end
        
    end

    function createEmptyCouplerFunctions()
    
        cpl_funcs = CouplerFunctions()

        cpl_funcs.master_before_model_init = function()
            return read_restart, cesm_coupler_time, Δt
        end

        cpl_funcs.master_after_model_init! = function(OMMODULE, OMDATA)
        end

        cpl_funcs.master_before_model_run! = function(OMMODULE, OMDATA)
        end

        cpl_funcs.master_after_model_run! = function(OMMODULE, OMDATA)
        end

        cpl_funcs.master_finalize! = function(OMMODULE, OMDATA)
        end 

        return cpl_funcs
    end

end


