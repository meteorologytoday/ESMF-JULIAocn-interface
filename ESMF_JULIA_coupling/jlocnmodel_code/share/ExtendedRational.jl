module ExtendedRationalModule

    using Printf

    export ExtendedRational
    export regularize, regularize!, string

    mutable struct ExtendedRational <: Real
        # Actuall number is n = s * ( i + f ) where i >= 0 and f >= 0
        # sign, integer, fraction
        s :: Int  # ture = pos , false = neg
        i :: Int
        f :: Rational{Int}
    end
    
    # ===================================================
    # Outer constructors
    function ExtendedRational(_i :: Int)
        s = sign(_i)
        i = (_i >= 0) ? _i : - _i
        f = Rational(0)
        return ExtendedRational(s, i, f)
    end

    function ExtendedRational(_f :: Rational{Int})
        
        s = sign(_f)
        i = 0
        f *= s
        
        era = new(s, i, f)
        regularize!(era)
    end 

    function ExtendedRational(era :: ExtendedRational)
        new_era = ExtendedRational(era.s, era.i, era.f)
        regularize!(new_era)
        return new_era
    end 

    # ===================================================
    
    function regularize!(era :: ExtendedRational)

        num = numerator(era.f)
        den = denominator(era.f)            
        
        if num >= den
            inc = floor(Int, num / den)
            era.i += inc
            era.f -= inc
        end
        
    end

    function regularize(era :: ExtendedRational)
        return ExtendedRational(era) # automatically regularize
    end

    function string(era :: ExtendedRational)
        return @sprintf("(%d, %d, %s)", era.s, era.i, string(era.f))
    end
    
    function (+)(a :: ExtendedRational, b :: ExtendedRational)
        
        if a.sign == b.sign
            
            regularize
            new_s
            new_i = a.i + b.i
            new_raw_f = a.f + b.f
            
        end 
    
        new_era = ExtendedRational()
        
        return ExtendedRational()
    end

end
