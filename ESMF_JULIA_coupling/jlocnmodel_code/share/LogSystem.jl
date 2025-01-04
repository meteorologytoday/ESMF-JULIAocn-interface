module LogSystem

    using Printf
    
    export writeLog, LogHandle, createLogHandle, errorLog
   

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

    function errorLog(
        lh :: LogHandle,
        fmt,
        args...;
        f :: String  = "??",
        l :: Integer = 0,
        force :: Bool = true,
    )
        s = writeLog(fmt, args... ; f = f, l = l)
        throw(ErrorException(s))
    end

    function writeLog(
        lh :: LogHandle,
        fmt,
        args...;
        f :: String = "??",
        l :: Integer = 0,
        detail :: Bool = false,
        force :: Bool = false,
    )
        if force || lh.rank == 0
            s = formatMsg(fmt, args... ; f = f, l = l, detail = detail)
            @printf("%s\n", s)
            return s
        end
    end

    function formatMsg(
        fmt, args...; f :: String, l :: Integer, detail::Bool,
    )
        s = Printf.format(Printf.Format(fmt), args...)

        if detail
            s = @sprintf(
                "[%s ; LN=%d] %s",
                f,
                l,
                s,
            )
        end

        return s
    end
end
