from mpi4py import MPI
import toml
from pprint import pprint


class OceanModel:
    
    def __init__(self, rank, comm):

        print("You are in the OceanModel constructor (ta-da)!")
        
        self.comm = comm 
        self.rank = self.comm.Get_rank()
        self.config_grid = toml.load("config_grid.toml")

        number_of_pet = self.comm.Get_size()
        
        nSx = cf["nSx"]
        nSy = cf["nSy"]
        nPx = cf["nPx"]
        nPy = cf["nPy"]
        Nx = cf["Nx"]
        Ny = cf["Ny"]
        Nr = cf["Nr"]

        total_hgrid = Nx * Ny
        
        if Nx % (nSx*nPx) != 0:
            self.error("Nx should be a multiple of nSx * nPx.")
 
        if Ny % (nSy*nPy) != 0:
            self.error("Ny should be a multiple of nSy * nPy.")
        

        #if nSx != 1 or nSy != 1 :
        #    raise Error("Currently I only support nSx = nSy = 1.")

        expected_number_of_pet = nPx * nPy
        if number_of_pet != expected_number_of_pet:
            self.error("expected_number_of_pet = %d but I only have %d pet" % (expected_number_of_pet, number_of_pet,))
        
        grids_per_tile_x = cf["Nx"] // (nPx * nSx)
        grids_per_pet_x = grids_per_tile_x * nSx

        grids_per_tile_y = cf["Ny"] // (nPx * nSx)
        grids_per_pet_y = grids_per_tile_y * nSy


        pet_idx_y = self.rank // grids_per_pet_x
        pet_idx_x = self.rank % grids_per_pet_x

        self.myXGlobalLo = [ grids_per_pet_x * pet_idx_x + grids_per_tile_x * i for i in range(nSx) ]
        self.myYGlobalLo = [ grids_per_pet_y * pet_idx_y + grids_per_tile_y * j for j in range(nSy) ]



    def report(self):
 
        print(f'Python: Rank {self.comm.Get_rank()} of {self.comm.Get_size()}')


        if self.rank == 0:
            self.printlog("Printing config_grid: ")
            pprint(self.config_grid, indent=4)

            self.printlog("myXGlobalLo = ", self.myXGlobalLo)
            self.printlog("myYGlobalLo = ", self.myYGlobalLo)
        


    def printlog(self, fmtstr, *args):
        
        s = fmtstr % args
        print("[Rank=%d/%d] %s" % (self.rank, self.comm.Get_size(), s))
    
    def error(self, fmtstr, *args):

        s = fmtstr % args
        s = "[Rank=%d/%d] Error: %s" % (self.rank, self.comm.Get_size(), s)
        print(s)
        raise Exception(s)
 

    def getDomainInfo(self):

        cf = self.config_grid
        
        
        myXGlobalLo = self.rank * 5   
        myYGlobalLo = self.rank * 10   
     
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

       
        



if __name__ == "__main__":
 
    rank = MPI.COMM_WORLD.Get_rank()
    size = MPI.COMM_WORLD.Get_size()
  
    model = OceanModel(rank, 1)
 
    print("Rank = ", rank)
    print("Size = ", size)
    
    print(model.getDomainInfo()) 
