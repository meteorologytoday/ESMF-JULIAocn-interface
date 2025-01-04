using Printf
using TOML
using CFTime

@printf("Running %s\n", @__FILE__)


@printf("This program emulate what ESMF is doing, serving as a debugging tool.\n")
@printf("This program also has the potential to serve as a future prototype of Julia centered coupler.\n")


include("../MPITools/MPI_essentials.jl")

if !(:DriverModule in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "DriverModule.jl")))
end


if !(:CouplingModule in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "Interface", "CouplingModule.jl")))
end

if !(:LogSystem in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "share", "LogSystem.jl")))
end

if !(:ModelTimeManagement in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "share", "ModelTimeManagement.jl")))
end

if !(:TimeTools in names(Main))
    include(normpath(joinpath(@__DIR__, "..", "share", "TimeTools.jl")))
end



using .LogSystem
using .CouplingModule
using .DriverModule
using .ModelTimeManagement
using .TimeTools

function setModelTimeInformation!(
    dr,
    caltype_str,
    begtime_str,
    endtime_str,
    timeinterval_s,
)
    caltype_mapping = Dict(
        "GREGORIAN" => CFTime.DateTimeProlepticGregorian,
        "JULIAN"    => CFTime.DateTimeJulian,
        "360DAY"    => CFTime.DateTime360Day,
        "NOLEAP"    => CFTime.DateTimeNoLeap,
    )

    println("[setModelTimeInformation!] caltype_str = [", caltype_str, "]") 

    if ! (caltype_str in keys(caltype_mapping))
        println("[setModelTimeInformation!] Exception happens")
        throw(ErrorException("Error: Calendar type `$caltype_str` is not supported."))
    end
    
    println("[setModelTimeInformation!] begtime_str = ", begtime_str) 
    println("[setModelTimeInformation!] endtime_str = ", endtime_str) 
    println("timeinterval_s = ", timeinterval_s) 

    caltype = caltype_mapping[caltype_str]   
    parsed_beg_time = parseTime(begtime_str)
    parsed_end_time = parseTime(endtime_str)

    cal_beg_time = caltype(
            parsed_beg_time.Y,
            parsed_beg_time.m,
            parsed_beg_time.d,
            parsed_beg_time.H,
            parsed_beg_time.M,
            parsed_beg_time.S,
    )

    cal_end_time = caltype(
            parsed_end_time.Y,
            parsed_end_time.m,
            parsed_end_time.d,
            parsed_end_time.H,
            parsed_end_time.M,
            parsed_end_time.S,
    )

    timedelta = cal_end_time - cal_beg_time
    niters = ceil(Int64, timedelta / Second(timeinterval_s) )

    mtc = ModelTimeConfig(0, timeinterval_s // 1)
    beg_time = ModelTime(mtc, 0)
    end_time = addIters(beg_time, niters)
    cur_time = copy_partial(beg_time)

    calendar = ModelCalendar(
        caltype_str,
        beg_time,
    )
    
    setClock!(
        dr.OMDATA.clock;
        beg_time = beg_time,
        end_time = end_time,
        cur_time = cur_time,
        calendar = calendar,
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



