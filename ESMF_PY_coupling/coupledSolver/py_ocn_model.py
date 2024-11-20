from mpi4py import MPI

class OceanModel:
    
    def __init__(self, rank, comm_id):

        print("You are in the OceanModel constructor!")
        self.rank = rank
        self.comm_id = comm_id
        self.comm = MPI.COMM_WORLD

        self.report()

    def report(self):
 
        if self.comm_id != 0:
            print("Warning: comm_id is not zero. This is not COMM_WORLD.")

       
        print(f'Python: Rank {self.comm.Get_rank()} of {self.comm.Get_size()}')





if __name__ == "__main__":
 
    rank = MPI.COMM_WORLD.Get_rank()
    size = MPI.COMM_WORLD.Get_size()
  
    model = OceanModel(rank, 1)
 
    print("Rank = ", rank)
    print("Size = ", size)
    
