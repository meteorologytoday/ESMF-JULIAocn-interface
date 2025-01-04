


include("../../share/ModelTimeManagement.jl")


using .ModelTimeManagement

mtc = ModelTimeConfig(0.0, 3600//1)
beg_time = ModelTime(mtc, 0)
end_time = ModelTime(mtc, 10)
cur_time = copy_partial(beg_time)

clock = ModelClock(
    "myclock",
)

setClock!(
    clock;
    time_cfg = mtc,
    beg_time = beg_time,
    end_time = end_time,
    cur_time = cur_time,
)

checkClock!(clock)
println(clock.status)

addAlarm!(
    clock,
    "begin",
    addIters(cur_time, 0),
    1;
    callback = nothing,
    recurring = 3,
)

for i=1:10
    println("[i=$i]")
    advanceClock!(clock, 1)
end
