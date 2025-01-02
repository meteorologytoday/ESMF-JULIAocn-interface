


module ESMF_Calendar

    const MONTHS_PER_YEAR = 12
    const days_of_month      = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    const days_of_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    const days_of_year = sum(days_of_month)
    const days_of_year_leap = sum(days_of_month_leap)


    #      INTEGER :: mdaycum(0:MONTHS_PER_YEAR)
    #  INTEGER :: mdayleapcum(0:MONTHS_PER_YEAR)
    #  TYPE(ESMF_BaseTime), TARGET :: monthbdys(0:MONTHS_PER_YEAR)
    #  TYPE(ESMF_BaseTime), TARGET :: monthbdysleap(0:MONTHS_PER_YEAR)


    # A Julia translation of 
    # https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/sref.v7.1.6/wrf_shared.v1.1.0/external/esmf_time_f90/ESMF_Calendar.f

    @enum ESMF_CalendarType begin

        CALKIND_GREGORIAN   = 1
        CALKIND_JULIAN      = 2
        CALKIND_NOLEAP      = 3
        CALKIND_360DAY      = 4
        
        # User defined
        CALKIND_GENERIC     = 5

        # track base time seconds only
        CALKIND_NOCALENDAR  = 6 
    end


    mutable struct ESMF_DaysPerYear

        # whole days per year
        D :: Integer
        
        # Fractional days-per-year are not yet used in this implementation.
        Dn :: Integer    # fractional days per year numerator
        Dd :: Integer    # fractional days per year denominator
        
    end


    mutable struct ESMF_Calendar
        
        is_set           :: Bool
        calendar_type    :: ESMF_CalendarType 
        days_per_month   :: AbstractArray{Integer, 1}   
        seconds_per_day  :: Integer
        seconds_per_year :: Integer
        days_per_year    :: ESMF_DaysPerYear


        function ESMF_Calendar(
            calendar_type :: ESMF_Calendar,
            days_per_month :: Union{Integer, AbstractArray{Integer, 1}},
            seconds_per_day :: Integer,
            days_per_year :: ESMF_DaysPerYear,
        )


            return new(
                true,
                calendar_type,
                days_per_month,
                seconds_per_day,
                seconds_per_year,
                days_per_year,
            )

        end 

    end

    mutable struct ESMFTime





    end

end
