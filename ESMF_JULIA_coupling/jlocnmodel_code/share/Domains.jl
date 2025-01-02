module Domains

    using Printf

    mutable struct Domain
        rank :: Integer
        number_of_pet :: Integer
        sNx :: Integer
        sNy :: Integer
        OLx :: Integer
        OLy :: Integer
        nSx :: Integer
        nSy :: Integer
        nPx :: Integer
        nPy :: Integer
        Nx  :: Integer
        Ny  :: Integer
        Nz  :: Integer
        myXGlobalLo :: Union{Nothing, Array{Integer}}
        myYGlobalLo :: Union{Nothing, Array{Integer}}
    end

    function setDomain!(
        domain :: Domain;
        rank   :: Integer,
        number_of_pet :: Integer,
    )

        domain.rank = rank
        domain.number_of_pet = number_of_pet
        
        sNx = domain.sNx
        sNy = domain.sNy
        nSx = domain.nSx
        nSy = domain.nSy
        nPx = domain.nPx
        nPy = domain.nPy
        Nx = domain.Nx
        Ny = domain.Ny
        Nz = domain.Nz

        total_hgrid = Nx * Ny
        
        if Nx != sNx * nSx * nPx
            throw(ErrorException("Nx must equal to sNx * nSx * nPx."))
        end

        if Ny != sNy * nSy * nPy
            throw(ErrorException("Ny must equal to sNy * nSy * nPy."))
        end        

        if nSx != 1 || nSy != 1
            throw(ErrorException("Currently I only support nSx = nSy = 1."))
        end

        expected_number_of_pet = nPx * nPy
        if number_of_pet != expected_number_of_pet
            throw(ErrorException(@sprintf("expected_number_of_pet = %d but I only have %d pet", expected_number_of_pet, number_of_pet,)))
        end
 
        grids_per_pet_x = sNx * nSx
        grids_per_pet_y = sNy * nSy

        pet_idx_y = floor(Int64, rank / nPx)
        pet_idx_x = rank % nPx


        domain.myXGlobalLo = [ grids_per_pet_x * pet_idx_x + sNx * i for i=1:nSx ]
        domain.myYGlobalLo = [ grids_per_pet_y * pet_idx_y + sNy * j for j=1:nSy ]

    end

end
