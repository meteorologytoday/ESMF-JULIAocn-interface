if !(:LogSystem in names(Main))
    include(normpath(joinpath(@__DIR__, "LogSystem.jl")))
end
if !(:TimeTools in names(Main))
    include(normpath(joinpath(@__DIR__, "TimeTools.jl")))
end






module ModelTimeManagement

    export ModelTimeModule
    export ModelTime, ModelTimeConfig
    export setIteration!, string, collapseTime
    export copy_partial, copy_full

    export ModelClockModule
    export ModelAlarm, ModelClock, advanceClock!, setClock!, addAlarm!, clock2str, dt2str, dt2tuple, dropRungAlarm!, checkClock!
    export ModelCalendar, setClockComprehensive!, toCalendarDatetime, ifClockEnds
    export addIters

    module ModelTimeModule
        
        export ModelTime, ModelTimeConfig
        export setIteration!, string, collapseTime
        export copy_partial, copy_full
        export addIters
        export isConsistent

        using Printf

    
        mutable struct ModelTimeConfig
            # time = ref + dt * iter
            ref  :: Float64 # The reference where time
            dt   :: Rational{Int64}
        end

       
        # abstime = ref + dt * iter
        mutable struct ModelTime
            cfg  :: ModelTimeConfig
            iter :: Int64   # Iteration  
        end
 

        function copy_partial(mt :: ModelTime)
            new_mt = ModelTime(mt.cfg, mt.iter+0)
            return new_mt
        end 

        function string(mt :: ModelTime)
            return @sprintf(
                "(ref=%f, dt=%s, iter=%d)",
                mt.cfg.ref,
                Base.string(mt.cfg.dt),
                mt.iter,
            )
        end 

        function collapseTime(mt :: ModelTime)
            return mt.cfg.ref + Float64(mt.cfg.dt * mt.iter)
        end 

        function isConsistent(a :: ModelTime, b::ModelTime)
            
            if a.cfg === b.cfg || ( a.cfg.ref === b.cfg.ref && a.cfg.dt == b.cfg.dt )
                return true
            else
                return false
            end
            
        end

        function Base.:(>)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
            if isConsistent(a, b)
                return a.iter > b.iter
            else
                return ModelTimeModule.collapseTime(a) > ModelTimeModule.collapseTime(b)
            end
        end

        function Base.:(<)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
            if isConsistent(a, b)
                return a.iter < b.iter
            else
                return ModelTimeModule.collapseTime(a) < ModelTimeModule.collapseTime(b)
            end
        end

        function Base.:(==)(a :: ModelTimeModule.ModelTime, b :: ModelTimeModule.ModelTime)
            if isConsistent(a, b)
                return a.iter == b.iter
            else
                return ModelTimeModule.collapseTime(a) == ModelTimeModule.collapseTime(b)
            end
        end
 
        function addIters(a :: ModelTimeModule.ModelTime, niter :: Integer)
            new_a = copy_partial(a)
            new_a.iter += niter
            return new_a
        end
 
        Base.string(mt :: ModelTimeModule.ModelTime) = ModelTimeModule.string(mt)
        #Base.copy(mt :: ModelTimeModule.ModelTime) = ModelTimeModule.copy(mt)



    end

    module ModelClockModule

        export ModelAlarm, ModelClock, advanceClock!, setClock!, addAlarm!, clock2str, dt2str, dt2tuple, dropRungAlarm!, checkClock!
        export ModelCalendar, setClockComprehensive!, toCalendarDatetime, ifClockEnds

        using Printf
        using ..ModelTimeModule 
        using CFTime, Dates
        using ...LogSystem
        using ...TimeTools

        mutable struct ModelAlarm
            name       :: String
            model_time :: ModelTime  #
            priority   :: Integer   # There might be multiple alarms ring at the same time. And if order is important, then the higher the number the higher the priority.
            callbacks :: Array{Function, 1}
            recurring :: Union{Nothing, Function, Integer}
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

         mutable struct ModelCalendar
            caltype  :: Any
            # calendar_ref defines what 
            # date does the ModelTime = 0
            # correspond to
            beg_date :: Any
        end

        mutable struct ModelClock
            
            name         :: String
            
            time_cfg     :: Union{ModelTimeConfig, Nothing}
            beg_time     :: Union{ModelTime, Nothing}
            end_time     :: Union{ModelTime, Nothing}
            cur_time     :: Union{ModelTime, Nothing}

            alarms       :: Array{ModelAlarm, 1}
            alarms_dict  :: Dict
            alarm_ptr    :: Integer
            calendar     :: Union{ModelCalendar, Nothing} 
            
            status :: Symbol

            log_handle :: LogHandle

            function ModelClock(
                name :: String;
                log_handle :: LogHandle,
            )
                alarms = Array{ModelAlarm}(undef, 0)
                alarms_dict = Dict()
                return new(
                    name,
                    nothing,
                    nothing,
                    nothing,
                    nothing,
                    alarms,
                    alarms_dict,
                    0,
                    nothing,
                    :UNSET,
                    log_handle,
                )
            end

        end

        macro setter(s, v)
            quote
                if esc($v) != nothing
                    $s.$v = $v
                end
            end
        end


        function setClock!(
            mc :: ModelClock;
            time_cfg :: Union{ModelTimeConfig, Nothing} = nothing,
            beg_time :: Union{ModelTime, Nothing} = nothing,
            end_time :: Union{ModelTime, Nothing} = nothing,
            cur_time :: Union{ModelTime, Nothing} = nothing,
            calendar :: Union{ModelCalendar, Nothing} = nothing,
            status   :: Union{Symbol, Nothing} = nothing,
        )

            if time_cfg != nothing
                mc.time_cfg = time_cfg
            end

            if beg_time != nothing
                mc.beg_time = beg_time
            end

            if end_time != nothing
                mc.end_time = end_time
            end

            if cur_time != nothing
                mc.cur_time = cur_time
            end

            if calendar != nothing
                mc.calendar = calendar
            end

            if status != nothing
                mc.status = status
            end


        end

        #=
        macro testifset(s, v, pass)
            quote
                if $s.$v == nothing
                    println("The $v of the model clock is not set yet.")
                    $pass = false
                end
            end
        end
        =#
        function checkClock!(
            mc :: ModelClock
        )

            pass = true

            pass = all([
                mc.time_cfg != nothing,
                mc.beg_time != nothing,
                mc.end_time != nothing,
                mc.cur_time != nothing,
            ])

            if pass
                mc.status = :READY
            else
                mc.status = :UNSET
            end

            return pass
        end

        #=
        function setCalendar!(
            mc   :: ModelClock,
            cal  :: ModelCalendar,
        )
            mc.model_calendar = cal

        end


        function setClock!(
            mc   :: ModelClock,
            iter :: Int64,
        )
            mc.model_time.iter = iter

        end
        =#

        function advanceClock!(
            mc :: ModelClock,
            diter :: Int64,
        )
            
            mc.cur_time.iter += diter
            # Only check alarms when there is any
            if length(mc.alarms) > 0
                
                if mc.alarm_ptr == 0 && mc.cur_time >= mc.alarms[1].model_time
                    mc.alarm_ptr = 1
                end
                
                while (0 < mc.alarm_ptr <= length(mc.alarms)) && (mc.cur_time >= mc.alarms[mc.alarm_ptr].model_time)
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
                writeLog(mc.log_handle, "Alarm '%s' rings at %s", alarm.name, dt2str(alarm.model_time))

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
                        next_time = addIters(alarm.model_time, alarm.recurring)
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
            return dt2str(mc.cur_time)
        end

        function dt2str(model_time :: ModelTime)
            return ModelTimeModule.string(model_time) #@sprintf("%04d-%02d-%02d %02d:%02d:%02d", dt2tuple(dt)...)
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
            recurring :: Union{ Nothing, Function, Integer} = nothing,
        )

            cur_time = mc.cur_time
            # If it is an integer, treat as iter
            if typeof(model_time) <: Int64
                model_time = ModelTime(cur_time.cfg, model_time) 
            end

            if ! ModelTimeModule.isConsistent(cur_time, model_time)
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

            if alarm.model_time < mc.cur_time 
                writeLog(mc.log_handle, "alarm.model_time = %s", dt2str(alarm.cur_time))
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
            if alarm.model_time == mc.cur_time
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
                writeLog(mc.log_handle, "Alarm[$(i)] = $(alarm.time). [$( (alarm.done) ? "v" : " " )] $( (i==mc.alarm_ptr) ? "*" : "" )")
            end
        end

        function setClockComprehensive!(
            mc            :: ModelClock;
            caltype       :: Union{String, Nothing} = nothing,
            beg_time      :: Union{String, Nothing} = nothing,
            end_time      :: Union{String, Nothing} = nothing,
            timestep_s    :: Rational{Int64},
            simlength_s   :: Union{ Rational{Int64}, Integer, Nothing} = nothing,
        )

            #println("[setModelTimeInformation!] caltype_str = [", caltype_str, "]")
     
            caltype_mapping = Dict(
                "GREGORIAN" => CFTime.DateTimeProlepticGregorian,
                "JULIAN"    => CFTime.DateTimeJulian,
                "360DAY"    => CFTime.DateTime360Day,
                "NOLEAP"    => CFTime.DateTimeNoLeap,
            )

            if caltype === nothing || caltype === "DEFAULT"
                caltype = "GREGORIAN"
                writeLog(mc.log_handle, "caltype not provided or is DEFAULT, set it as %s", caltype)
            end

            if ! (caltype in keys(caltype_mapping))
                throw(ErrorException("Error: Calendar type `$caltype` is not supported."))
            end

            caltype = caltype_mapping[caltype]
            
            if beg_time === nothing
                beg_time = "0001-01-01T00:00:00"
                writeLog(mc.log_handle, "beg_time not provided, set it as %s", beg_time)
            end

            writeLog(mc.log_handle, "[setClockComprehensive!] beg_time = %s", beg_time) 
            parsed_beg_time = parseTime(beg_time)
            beg_time = caltype(
                    parsed_beg_time.Y,
                    parsed_beg_time.m,
                    parsed_beg_time.d,
                    parsed_beg_time.H,
                    parsed_beg_time.M,
                    parsed_beg_time.S,
            )

            if end_time === nothing && simlength_s === nothing
                throw(ErrorException("Error: Either `simlength_s` or `end_time` has to be provided"))
            end

            if simlength_s !== nothing
                writeLog(mc.log_handle, "Use `simlength_s` to determine simulation end time.")
                end_time = beg_time + Second(simlength_s)
            else
                writeLog(mc.log_handle, "Use `end_time` to determine simulation end time.")
                parsed_end_time = parseTime(end_time)
                end_time = caltype(
                        parsed_end_time.Y,
                        parsed_end_time.m,
                        parsed_end_time.d,
                        parsed_end_time.H,
                        parsed_end_time.M,
                        parsed_end_time.S,
                )
            end
            
            timedelta = end_time - beg_time
            
            niters = timedelta / Second(timestep_s)

            if niters % 1 != 0
                errorLog(log_handle, "Error: The computed timedelta = %s, and it must be an integer multiple of `timestep_s`", string(timedelta); f = @__FILE__, l = @__LINE__)
            end

            niters = Int64(niters)

            mtc = ModelTimeConfig(0, timestep_s)

            beg_time_mt = ModelTime(mtc, 0)
            end_time_mt = addIters(beg_time_mt, niters)
            cur_time_mt = copy_partial(beg_time_mt)

            calendar = ModelCalendar(
                caltype,
                beg_time,
            )
            
            setClock!(
                mc;
                time_cfg = mtc,
                beg_time = beg_time_mt,
                end_time = end_time_mt,
                cur_time = cur_time_mt,
                calendar = calendar,
            )

            pass = checkClock!(mc)
            
            if ! pass
                errorLog("Something is wrong when setting Clock."; f=@__FILE__, l=@__LINE__)
            end
            
        end

        function toCalendarDatetime(
            mt  :: ModelTime,
            cal :: ModelCalendar,
        )
            return cal.beg_date + Second(mt.iter * mt.cfg.dt)
        end

        function ifClockEnds(
            mc :: ModelClock,
        )
            return mc.cur_time.iter == mc.end_time.iter 
        end
    end

    using .ModelTimeModule
    using .ModelClockModule

end
