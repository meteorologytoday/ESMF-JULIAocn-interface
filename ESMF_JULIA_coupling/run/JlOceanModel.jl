
module OceanModel

    using MPI
    using TOML

    mutable struct Env
        
        model_config_file :: String
        model_config :: Dict

        function Env(
            model_config_file :: String ="model_config.toml",
        )

            println("Parsing file: ", model_config_file)
            model_config = TOML.parsefile(model_config_file)
            
            return new(
                model_config_file,
                model_config,
            )
        end
    end

    mutable struct State
        function State(env)
            return new()
        end
    end

    mutable struct Core
        function Core(env)
            return new()
        end
    end

    mutable struct Model

        env   :: Env
        state :: State 
        core  :: Core
        comm  :: MPI.Comm
        
        function Model(
            env :: Env;
            comm :: Union{MPI.Comm, Nothing} = nothing,
        )
        
            println("You are in the OceanModel constructor (ta-da)!")
            
            state = State(env)
            core  = Core(env)

            if comm == nothing
                println("Communicator not received. Using default : MPI.COMM_WORLD")
                comm = MPI.COMM_WORLD
            end
            
            o = new(
                env,
                state,
                core,
                comm,
            )

            return o

        end



    end

    function report(m :: Model)
        println("MPI information:")
        println(" - MPI : ", m.comm)
        println(" - MPI Size: ", MPI.Comm_size(m.comm))
        println(" - MPI Rank: ", MPI.Comm_rank(m.comm))
        
        println("End of Report")
    end

    function createOceanModel(
        config_file :: String;
        comm :: Union{MPI.Comm, Nothing} = nothing,
    )

        model = nothing

        try
            println("Initiating Env...")
            
            env = OceanModel.Env(config_file)
            model = OceanModel.Model(env ; comm=comm)
            println("Report ocean model...")
            OceanModel.report(model)

        catch e

            println("Exception occurs: ", e) 
            
        end

        if model == nothing
            println("Model is `nothing`. There must be some error during model creation. Please check.")
        end
        
        return model
    end
end



#=        
        self.initGrid()

    def initGrid(self):
 
        cf = self.config_grid
 
        sNx = cf["sNx"]
        sNy = cf["sNy"]
        nSx = cf["nSx"]
        nSy = cf["nSy"]
        nPx = cf["nPx"]
        nPy = cf["nPy"]
        Nx = cf["Nx"]
        Ny = cf["Ny"]
        Nr = cf["Nr"]
         
        mask_sT  = np.array(())
        da_sT    = np.zeros_like(mask_sT)
        dxU_sT   = np.zeros_like(mask_sT)
        dyV_sT   = np.zeros_like(mask_sT)


        self.vars = dict(
            
        ) 
        

    def checkGrid(self):
        
        number_of_pet = self.comm.Get_size()
 
        cf = self.config_grid
 
        sNx = cf["sNx"]
        sNy = cf["sNy"]
        nSx = cf["nSx"]
        nSy = cf["nSy"]
        nPx = cf["nPx"]
        nPy = cf["nPy"]
        Nx = cf["Nx"]
        Ny = cf["Ny"]
        Nr = cf["Nr"]

        total_hgrid = Nx * Ny
        
        if Nx != sNx * nSx * nPx:
            self.error("Nx must equal to sNx * nSx * nPx.")
 
        if Ny != sNy * nSy * nPy:
            self.error("Ny must equal to sNy * nSy * nPy.")
        

        if nSx != 1 or nSy != 1 :
            self.error("Currently I only support nSx = nSy = 1.")

        expected_number_of_pet = nPx * nPy
        if number_of_pet != expected_number_of_pet:
            self.error("expected_number_of_pet = %d but I only have %d pet" % (expected_number_of_pet, number_of_pet,))
        
        grids_per_pet_x = sNx * nSx
        grids_per_pet_y = sNy * nSy

        pet_idx_y = self.rank // nPx
        pet_idx_x = self.rank % nPx


        self.myXGlobalLo = [ grids_per_pet_x * pet_idx_x + sNx * i for i in range(nSx) ]
        self.myYGlobalLo = [ grids_per_pet_y * pet_idx_y + sNy * j for j in range(nSy) ]



    def report(self):
 
        print(f'Python: Rank {self.comm.Get_rank()} of {self.comm.Get_size()}')


        if self.rank == 0:
            self.printlog("Printing config_grid: ")
            pprint(self.config_grid, indent=4)

        self.printlog("myXGlobalLo = %s", str(self.myXGlobalLo))
        self.printlog("myYGlobalLo = %s", str(self.myYGlobalLo))
    


    def printlog(self, fmtstr, *args):
       
        if len(args) != 0: 
            s = fmtstr % args

        else:
            s = fmtstr
        print("[Rank=%d/%d] %s" % (self.rank, self.comm.Get_size(), s))
    
    def error(self, fmtstr, *args):

        s = fmtstr % args
        s = "[Rank=%d/%d] Error: %s" % (self.rank, self.comm.Get_size(), s)
        print(s)
        raise Exception(s)
 

    def getDomainInfo(self):

        cf = self.config_grid
        
       
        # For now we only support nSx = nSy = 1 
        myXGlobalLo = self.myXGlobalLo[0]
        myYGlobalLo = self.myYGlobalLo[0]
     
        return (
            cf["sNx"],
            cf["sNy"],
            cf["OLx"],
            cf["OLy"],
            cf["nSx"],
            cf["nSy"],
            cf["nPx"],
            cf["nPy"],
            cf["Nx"],
            cf["Ny"],
            cf["Nr"],
            myXGlobalLo,
            myYGlobalLo,
        )

       

    def getExportStates(self):
        
        arr = np.zeros((3, 4, 5), dtype=np.int32)
        arr[:] = np.arange(60).reshape(arr.shape)
        return arr 



if __name__ == "__main__":
 
    rank = MPI.COMM_WORLD.Get_rank()
    size = MPI.COMM_WORLD.Get_size()
  
    model = OceanModel(rank, MPI.COMM_WORLD, "pyocnmodel_code/config_grid.toml")
 
    print("Rank = ", rank)
    print("Size = ", size)
    
    print(model.getDomainInfo()) 
    model.report()

    model.getExportStates()


def getDomainInfo(self):

    cf = self.config_grid
    
   
    # For now we only support nSx = nSy = 1 
    myXGlobalLo = self.myXGlobalLo[0]
    myYGlobalLo = self.myYGlobalLo[0]
 
    return (
        cf["sNx"],
        cf["sNy"],
        cf["OLx"],
        cf["OLy"],
        cf["nSx"],
        cf["nSy"],
        cf["nPx"],
        cf["nPy"],
        cf["Nx"],
        cf["Ny"],
        cf["Nr"],
        myXGlobalLo,
        myYGlobalLo,
    )

end
=#
