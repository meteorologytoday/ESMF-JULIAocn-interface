module LogSystem

    using Printf
    
    export writeLog, LogHandle, createLogHandle
   

    mutable struct LogHandle
        rank :: Integer
        output_file :: String
    end
    
    function createLogHandle(
        rank :: Integer,
        output_dir :: String = ".",
    )

        filename = @sprintf("log.%04d", rank)
        return LogHandle(
            rank,
            joinpath(output_dir, filename)
        )
        
    end

    function writeLog(
        lh :: LogHandle,
        fmt,
        args...;
        force :: Bool = false,
    )
        if force || lh.rank == 0
            s = Printf.format(Printf.Format(fmt), args...)
            @printf("%s\n", s)
        end
    end
end
