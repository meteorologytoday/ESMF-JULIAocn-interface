

function getVariable(
    model    :: METADATA,
    category :: String,
    varname  :: String,
) 

    mb = model.mb
    fi = mb.fi
    var = nothing
    valid_yrng = model.mb.ev.valid_yrng
    
    if category == "COORD"
        gd_slab = model.mb.ev.gd_slab
        if varname == "LATITUDE"
            var = gd_slab.ϕ_T[:, :, valid_yrng][:]
        elseif varname == "LONGITUDE"
            var = gd_slab.λ_T[:, :, valid_yrng][:]
        end
    elseif category == "COMP2CPL"
        if varname == "sst"
            var = model.o2x["SST"]
            println("SST = ", var[1, 1, :]) 
        end
    elseif category == "CPL2COMP"
        if varname == "rsns"
            var = model.x2o["SWFLX"]
        end
    end

    if var == nothing
        throw(ErrorException("Unknown category `$category` or varname `$varname`."))
    end 

    return var
end
