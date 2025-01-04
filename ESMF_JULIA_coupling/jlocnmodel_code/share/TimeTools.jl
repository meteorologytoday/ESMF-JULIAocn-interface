
module TimeTools

    export parseDateTime, parseTime
    
    function parseDateTime(
        timetype,
        str :: String,
    )

        m = match(r"(?<year>[0-9]+)-(?<month>[0-9]{2})-(?<day>[0-9]{2})\s+(?<hour>[0-9]{2}):(?<min>[0-9]{2}):(?<sec>[0-9]{2})", str)
        if m == nothing
            throw(ErrorException("Unknown time format: " * (str)))
        end

        return timetype(
            parse(Int64, m[:year]),
            parse(Int64, m[:month]),
            parse(Int64, m[:day]),
            parse(Int64, m[:hour]),
            parse(Int64, m[:min]),
            parse(Int64, m[:sec]),
        )
    end

    function parseTime(
        timestr :: String,
    )
        time_regex = r"(?<Y>\d+)-(?<m>\d+)-(?<d>\d+)T(?<H>\d+):(?<M>\d+):(?<S>\d+(?:\.\d*)?)"
        
        regex_match = match(time_regex, timestr)
        if regex_match !== nothing
            Y = parse(Int, regex_match["Y"])
            m = parse(Int, regex_match["m"])
            d = parse(Int, regex_match["d"])
            H = parse(Int, regex_match["H"])
            M = parse(Int, regex_match["M"])
            S = parse(Float64, regex_match["S"])
            
            if S % 1 != 0
                throw(ErrorException("I do not accept fractional seconds"))
            end
            
            S = Int(S)
        else
            throw(ErrorException("Not a valid time format: $timestr"))
        end

        return (Y=Y,m=m,d=d,H=H,M=M,S=S)
    end



end
