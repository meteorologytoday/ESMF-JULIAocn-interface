

mutable struct Grid
    
    pet_size :: Int64
    pet_rank :: Int64

    sNx :: Int64
    sNy :: Int64
    OLx :: Int64
    OLy :: Int64
    nSx :: Int64
    nSy :: Int64
    nPx :: Int64
    nPy :: Int64
    Nx  :: Int64
    Ny  :: Int64
    Nr  :: Int64

    myXGlobalLo :: AbstractArray{Int64, 1}
    myYGlobalLo :: AbstractArray{Int64, 1}

    mask_sT :: AbstractArray{Float64, 2}
    da_sT   :: AbstractArray{Float64, 2}
    dxU_sT  :: AbstractArray{Float64, 2}
    dyV_sT  :: AbstractArray{Float64, 2}

    function Grid(
        cfg :: Dict; 
        pet_size :: Int64,
        pet_rank :: Int64,
    )
       
        mask_sT = zeros(Float64, cfg["sNx"] * cfg["nSx"], cfg["sNy"] * cfg["nSy"])
        da_sT   = copy(mask_sT)
        dxU_sT  = copy(mask_sT)
        dyV_sT  = copy(mask_sT)

        myXGlobalLo = zeros(Int64, pet_size)
        myYGlobalLo = copy(myXGlobalLo)
 
        gd = new(

            pet_size,
            pet_rank,

            cfg["sNx"],
            cfg["sNy"],
            cfg["OLx"],
            cfg["OLy"],
            cfg["nSx"],
            cfg["nSy"],
            cfg["nPx"],
            cfg["nPy"],
            cfg["Nx"],
            cfg["Ny"],
            cfg["Nr"],
            myXGlobalLo,
            myYGlobalLo,
            mask_sT,
            da_sT,
            dxU_sT,
            dyV_sT,
        )

        setGlobalLo!(gd)


        return gd
        
    end

    function setGlobalLo!(gd :: Grid)


        sNx = gd.sNx
        sNy = gd.sNy
        nSx = gd.nSx
        nSy = gd.nSy
        nPx = gd.nPx
        nPy = gd.nPy
        Nx = gd.Nx
        Ny = gd.Ny
        Nr = gd.Nr

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
        
        if gd.pet_size != expected_number_of_pet
            println("expected_number_of_pet = $expected_number_of_pet but I only have $(gd.pet_size)")
        end
        
        grids_per_pet_x = sNx * nSx
        grids_per_pet_y = sNy * nSy
        
        pet_idx_y = floor(Int64, gd.pet_rank / nPx)
        pet_idx_x = gd.pet_rank % nPx
       
        for i=1:nSx 
            gd.myXGlobalLo[i] = grids_per_pet_x * pet_idx_x + sNx * (i-1)
        end
        
        for j=1:nSy
            gd.myYGlobalLo[j] = grids_per_pet_y * pet_idx_y + sNy * (j-1)
        end
        

    end

end
