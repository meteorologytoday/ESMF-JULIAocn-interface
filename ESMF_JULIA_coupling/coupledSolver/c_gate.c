#include <stdio.h>
#include <julia.h>
#include <mpi.h>

extern "C" {

void MARCOISCOOL_JLMODEL_INIT( int thread_id, int fcomm) {
    MPI_Comm comm = MPI_Comm_f2c(fcomm);

    jl_init();
    
    printf("[C Code] Loading Julia stuff...\n");
    (void) jl_eval_string("include(\"MPI_essentials.jl\")");


    printf("[C Code] Boxing the communicator...\n");
    // Convert the MPI communicator to a Julia value
    jl_value_t *comm_value = jl_box_int64((int64_t)comm);
    
    // Call a Julia function
    printf("[C Code] Calling passMPICommunicator\n");
    jl_function_t *func = jl_get_function(jl_main_module, "passMPICommunicator");
    jl_call1(func, comm_value);
    
    printf("[C Code] Loading JlOceanModel.jl\n");
    (void) jl_eval_string("include(\"JlOceanModel.jl\")");
    
    printf("[C Code] Done loading JlOceanModel.jl\n");
    printf("[C Code] ##########################\n");


}

void MARCOISCOOL_JLMODEL_FINAL( int thread_id, int fcomm) {

    printf("[C Code] Finalizing JULIA...\n");
    jl_atexit_hook(0);

}

void MARCOISCOOL_JLMODEL_getDomainInfo( \
    int*  sNx, \
    int*  sNy, \
    int*  OLx, \
    int*  OLy, \
    int*  nSx, \
    int*  nSy, \
    int*  nPx, \
    int*  nPy, \
    int*  Nx, \
    int*  Ny, \
    int*  Nr, \
    int*  myXGlobalLo, \
    int*  myYGlobalLo  \
) {
 
    


}


}
