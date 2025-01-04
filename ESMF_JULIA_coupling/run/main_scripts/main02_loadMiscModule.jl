using Printf
using TOML
using CFTime

@printf("Running %s\n", @__FILE__)


@printf("This program emulate what ESMF is doing, serving as a debugging tool.\n")
@printf("This program also has the potential to serve as a future prototype of Julia centered coupler.\n")


include("../MPITools/MPI_essentials.jl")
include("../Driver_generic.jl")

if !(:CouplingModule in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "Interface", "CouplingModule.jl")))
end

if !(:LogSystem in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "share", "LogSystem.jl")))
end

if !(:ModelTimeManagement in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "share", "ModelTimeManagement.jl")))
end


using .LogSystem
using .CouplingModule
using .DriverModule
using .ModelTimeManagement


global model_calendar

caltype_mapping = Dict(
    "GREGORIAN" => CFTime.DateTimeProlepticGregorian,
    "JULIAN"    => CFTime.DateTimeJulian,
    "360DAY"    => CFTime.DateTime360Day,
    "NOLEAP"    => CFTime.DateTimeNoLeap,
)

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


function setModelTimeInformation!(
    dr,
    caltype_str,
    begtime_str,
    endtime_str,
    timeinterval_s,
)
    println("[setModelTimeInformation!] caltype_str = [", caltype_str, "]") 

    if ! (caltype_str in keys(caltype_mapping))
        println("[setModelTimeInformation!] Exception happens")
        throw(ErrorException("Error: Calendar type `$caltype_str` is not supported."))
    end
    
    println("[setModelTimeInformation!] begtime_str = ", begtime_str) 
    println("[setModelTimeInformation!] endtime_str = ", endtime_str) 
    println("timeinterval_s = ", timeinterval_s) 
   
    begtime = parseTime(begtime_str)
    endtime = parseTime(endtime_str)
 
    model_calendar = ModelCalendar(
        caltype_str,
        caltype_mapping[caltype_str](
            begtime.Y,
            begtime.m,
            begtime.d,
            begtime.H,
            begtime.M,
            begtime.S,
        )
    )
    
    setCalendar!(
        dr.OMDATA.clock,
        model_calendar,
    )

    println("[setModelTimeInformation!] Exiting")
end



global XYZ
function createArray(varname, arr_size)
    println("[createArray] varname: ", varname, "; arr_size = ", arr_size)
    global XYZ = collect(Float64, range(1, arr_size))
    return XYZ
end

function printXYZ()
    println("!!!!!!! XYZ = ", XYZ)
end


@printf("[DRIVER] Obtain config from: %s\n", config_file)
config = TOML.parsefile(config_file)



# Next need to detect calendar type 
# and make a ModelCalendar



