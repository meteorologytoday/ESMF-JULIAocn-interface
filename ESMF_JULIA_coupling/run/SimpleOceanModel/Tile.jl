mutable struct Tile

    # TempField means the variable will not be in the 
    # snapshot. It can still be output in the hist file
    # but only for debugging usage.

    ev  :: Env
    fi  :: Field
    tmp :: TempField
    tb  :: Union{Toolbox, Nothing}

    function Tile(
        ev :: Env;
        init_toolbox :: Bool = false,
    ) 

        fi = Field(ev)
        tmpfi = TempField(ev)

        tile = new(
            ev,
            fi,
            tmpfi,
            nothing,
        )
        
        tb = (init_core) ? Core(ev, tmpfi) : nothing
        tile.tb = tb
        
        return tile

    end
end


