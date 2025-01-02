module Domains

    mutable struct Domain
        rank :: Integer
        number_of_pet :: Integer
        sNx :: Integer
        sNy :: Integer
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

    function checkDomain(
        domain :: Domain;
        rank   :: Integer,
        number_of_pet :: Integer,
    )
        
        sNx = domain.sNx
        sNy = sNy
        nSx = nSx
        nSy = nSy
        nPx = nPx
        nPy = nPy
        Nx = Nx
        Ny = Ny
        Nz = Nz

        total_hgrid = Nx * Ny
        
        if Nx != sNx * nSx * nPx:
            throw(ErrorException("Nx must equal to sNx * nSx * nPx."))
 
        if Ny != sNy * nSy * nPy:
            throw(ErrorException("Ny must equal to sNy * nSy * nPy."))
        

        if nSx != 1 or nSy != 1 :
            throw(ErrorException("Currently I only support nSx = nSy = 1."))

        expected_number_of_pet = nPx * nPy
        if number_of_pet != expected_number_of_pet
            throw(ErrorException(@sprintf("expected_number_of_pet = %d but I only have %d pet", expected_number_of_pet, number_of_pet,)))
        
        grids_per_pet_x = sNx * nSx
        grids_per_pet_y = sNy * nSy

        pet_idx_y = floor(Int64, rank / nPx)
        pet_idx_x = rank % nPx


        domain.myXGlobalLo = [ grids_per_pet_x * pet_idx_x + sNx * i for i=1:nSx ]
        domain.myYGlobalLo = [ grids_per_pet_y * pet_idx_y + sNy * j for j=1:nSy ]

    end

end
