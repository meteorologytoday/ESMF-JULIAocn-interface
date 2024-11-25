#include <mpi.h>
#include <stdio.h>
#include <julia.h>

int main(int argc, char *argv[]) {

    MPI_Init(&argc, &argv);

    jl_init();
    (void) jl_eval_string("using MPIPreferences");
    (void) jl_eval_string("MPIPreferences.use_system_binary(; library_names=[\"/home/t2hsu/miniconda3/envs/mpi/lib/libmpi\"]);");
    (void) jl_eval_string("using MPI");
    (void) jl_eval_string("println(\"Initializing...\")");
    (void) jl_eval_string("println(\"Done.\")");
    
    (void) jl_eval_string("if MPI.Initialized() ;  println(\"MPI is initialized.\") ; else ; println(\"Warning: MPI is not initialized.\") ; end ");
    
    (void) jl_eval_string("comm = MPI.COMM_WORLD");
    (void) jl_eval_string("println(comm)");
    (void) jl_eval_string("println(MPI.Comm_size(comm))");

    jl_atexit_hook(0);

    MPI_Finalize();

    return 0;
}




