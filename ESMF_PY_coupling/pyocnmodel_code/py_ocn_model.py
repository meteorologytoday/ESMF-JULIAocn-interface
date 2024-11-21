from mpi4py import MPI
import toml
from pprint import pprint


class OceanModel:
    
    def __init__(self, rank, comm_id):

        print("You are in the OceanModel constructor!")


        self.comm = MPI.COMM_WORLD
        self.comm_id = comm_id
        self.rank = self.comm.Get_rank()
        self.config_grid = toml.load("config_grid.toml")

    def report(self):
 
        if self.comm_id != 0:
            print("Warning: comm_id is not zero. This is not COMM_WORLD.")

       
        print(f'Python: Rank {self.comm.Get_rank()} of {self.comm.Get_size()}')


        if self.rank == 0:
            self.printlog("Printing config_grid: ")
            pprint(self.config_grid, indent=4)
        


    def printlog(self, fmtstr, *args):
        
        s = fmtstr % args
        print("[Rank=%d/%d] %s" % (self.rank, self.comm.Get_size(), s))


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
