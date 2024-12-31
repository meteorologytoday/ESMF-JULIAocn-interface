module ModelClockSystem

    export ModelAlarm, ModelClock, advanceClock!, setClock!, addAlarm!, clock2str, dt2str, dt2tuple, dropRungAlarm!

    using Printf
    using ..ModelTimeModule 

    mutable struct ModelAlarm
        name      :: String
        model_time      :: Float64  #
        priority  :: Integer   # There might be multiple alarms ring at the same time. And if order is important, then the higher the number the higher the priority.
        callbacks :: Array{Function, 1}
        recurring :: Union{Nothing, Function}
        done      :: Bool
    end

    function isEarlier(x :: ModelAlarm, y :: ModelAlarm)
        return (x.model_time < y.model_time) || ( (x.model_time == y.model_time) && (x.priority > y.priority))
    end

    function isEqual(x :: ModelAlarm, y :: ModelAlarm)
        return (x.model_time == y.model_time) && (x.priority == y.priority)
    end

    function isLater(x :: ModelAlarm, y :: ModelAlarm)
        return ! ( isEarlier(x, y) ||  isEqual(x, y) )
    end


    mutable struct ModelClock
        
        name         :: String
        model_time         :: ModelTime
        alarms       :: Array{ModelAlarm, 1}
        alarms_dict  :: Dict
        alarm_ptr    :: Integer 
        
        function ModelClock(
            name :: String,
            model_time :: ModelTime,
        )
            alarms = Array{ModelAlarm}(undef, 0)
            alarms_dict = Dict()
            return new(
                name,
                model_time,
                alarms,
                alarms_dict,
                0,
            )
        end

    end

    function setClock!(
        mc   :: ModelClock,
        iter :: Int64,
    )
        mc.model_time = iter

    end

    function advanceClock!(
        mc :: ModelClock,
        diter :: Int64,
    )
        
        mc.model_time.iter += diter
        
        # Only check alarms when there is any
        if length(mc.alarms) > 0
            
            if mc.alarm_ptr == 0 && mc.model_time >= mc.alarms[1].model_time
                mc.alarm_ptr = 1
            end
            
            while (0 < mc.alarm_ptr <= length(mc.alarms)) && (mc.model_time >= mc.alarms[mc.alarm_ptr].model_time)
            #    println("Before ring alarm: mc.alarm_ptr  = $(mc.alarm_ptr)")
                # In case we are at the end of alarm array
               
                alarm_ptr_tmp = mc.alarm_ptr 
                if mc.alarm_ptr < length(mc.alarms) 
                    mc.alarm_ptr += 1
                end 
                ringAlarm!(mc, mc.alarms[alarm_ptr_tmp])
             #   println("xxx alarm_ptr = $(mc.alarm_ptr)")
                if mc.alarm_ptr == length(mc.alarms) 
                    break
                end
            end
        end


    end

    function ringAlarm!(mc :: ModelClock, alarm :: ModelAlarm)
        #println("alarm.time = $(alarm.time). done = $(alarm.done)")
        if ! alarm.done
            println(@sprintf("Alarm '%s' rings at %s", alarm.name, dt2str(alarm.model_time)))

            for callback in alarm.callbacks
                callback(mc, alarm)
            end
            
            # IMPORTANT: alarm.done has to be here.
            # If it is after addAlarm!, there is a potential recursive loop because
            # advanceClock! might be called when calling addAlarm! (i.e. ring immediately)
            alarm.done = true

            if alarm.recurring != nothing
                
                if isa(alarm.recurring, Function)
                    next_time = alarm.recurring(alarm.model_time)
                else
                    next_time = alarm.model_time + alarm.recurring
                end
                #println("Current time: $(string(alarm.time))")
                #println("Next alarm: " * string(next_time))
                addAlarm!(
                    mc,
                    alarm.name,
                    next_time,
                    alarm.priority;
                    callback = alarm.callbacks,
                    recurring = alarm.recurring,
                )
            end
        end
    end

    function clock2str(mc :: ModelClock)
        return dt2str(mc.model_time)
    end

    function dt2str(model_time :: ModelTime)
        return string(model_time) #@sprintf("%04d-%02d-%02d %02d:%02d:%02d", dt2tuple(dt)...)
    end

    function dt2tuple(dt)
        return (Dates.year(dt), Dates.month(dt), Dates.day(dt), Dates.hour(dt), Dates.minute(dt), Dates.second(dt))
    end

    function addAlarm!(
        mc   :: ModelClock,
        name :: String,
        model_time :: Union{Int64, ModelTime},
        priority :: Integer;
        callback :: Union{Array{Function, 1}, Function, Nothing} = nothing,
        recurring :: Union{ Nothing, Function } = nothing,
    )


        # If it is an integer, treat as iter
        if typeof(model_time) <: Int64
            model_time = ModelTime(mc.model_time.cfg, model_time) 
        end

        if ! isConsistent(mc.model_time, model_time)
            throw(ErrorException("Inconsistent model_time while adding alarm."))
        end

        if callback == nothing
            callbacks = Array{Function, 1}(undef, 0)
        elseif typeof(callback) <: Function
            callbacks = [ callback ]
        else # it is already an array
            callbacks = callback
        end        

        alarm = ModelAlarm(
            name,
            model_time,
            priority,
            callbacks,
            recurring,
            false,
        )
        
        if mc.alarm_ptr == 0
            mc.alarm_ptr = 1
        end

        if alarm.model_time < mc.model_time 
            println("alarm.model_time = ", dt2str(alarm.model_time))
            throw(ErrorException("Alarm needs to be set in the future."))
        end

        push!(mc.alarms, alarm)
        if ! haskey(mc.alarms_dict, name)
            mc.alarms_dict[name] = []
        end



        if (mc.alarm_ptr > 1) && (isEarlier(alarm, mc.alarms[mc.alarm_ptr]) || isEqual(alarm, mc.alarms[mc.alarm_ptr]))
            mc.alarm_ptr -= 1
        end
        
        push!(mc.alarms_dict[name], alarm)
        # lt = less than = earlier and higher priority
        sort!(mc.alarms, lt = isEarlier)

        # What is this doing?
        # Ans: Fire off the alarm callback if the
        #      added alarm matches the current time
        if alarm.model_time == mc.model_time
            advanceClock!(mc, 0)
        end


    end

    function dropRungAlarm!(
        mc   :: ModelClock,
    )
        while length(mc.alarms) > 0 && mc.alarms[1].done
            alarm = popfirst!(mc.alarms)
            delete!(mc.alarms_dict, alarm.name)
            mc.alarm_ptr -= 1
            if mc.alarm_ptr < 0 
                throw(ErrorException("Error happens in alarm counts. Please check."))
            end
        end
    end

    function listAlarms(
        mc :: ModelClock,
    )
        for (i, alarm) in enumerate(mc.alarms)
            println("Alarm[$(i)] = $(alarm.time). [$( (alarm.done) ? "v" : " " )] $( (i==mc.alarm_ptr) ? "*" : "" )")
        end
    end
end
