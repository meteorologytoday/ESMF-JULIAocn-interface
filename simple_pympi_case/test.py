import mpi4py.MPI as MPI
#rank = MPI.COMM_WORLD.Get_rank()
#size = MPI.COMM_WORLD.Get_size()




# example.py
class OceanModel:
    
    def __init__(self, thread_id, comm_id):

        self.thread_id = thread_id
        self.comm_id = comm_id
        self.comm = MPI.Comm.id2comm(comm_id)

    def report(self):
        
        print(f'Python: Rank {self.comm.Get_rank()} of {self.comm.Get_size()}')


