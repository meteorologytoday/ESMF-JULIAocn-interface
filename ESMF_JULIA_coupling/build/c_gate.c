#include <stdio.h>
#include <julia.h>
#include <mpi.h>

extern "C" {

jl_value_t *ocean_model;

void MARCOISCOOL_JLMODEL_INIT( int thread_id, int fcomm) {

    
    int buffer_size = 4096;
    char cmd[buffer_size] = "";

    MPI_Comm comm = MPI_Comm_f2c(fcomm);

    int rank, size;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &size);
    printf("Hello from rank %d out of %d processes\n", rank, size);


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
 
    if (jl_exception_occurred()) {
        printf("Exception occurred after I pass MPI Communicator: %s\n", jl_string_ptr(jl_exception_occurred()));
    }
    
    printf("[C Code] Loading JlOceanModel.jl\n");
    (void) jl_eval_string("include(\"JlOceanModel.jl\")");
    printf("[C Code] Done loading JlOceanModel.jl\n");

    printf("[C Code] Initiating ocean model\n");
 
    if (jl_exception_occurred()) {
        printf("Exception occurred before initiating ocean model: %s\n", jl_string_ptr(jl_exception_occurred()));
    }
    
    snprintf(cmd, buffer_size, "Main.OceanModel.createOceanModel(\"model_config.toml\"; comm=COMM_ROOT)");
    printf("Going to eval: %s\n", cmd);
    ocean_model = (jl_value_t *) jl_eval_string(cmd);

    if (jl_exception_occurred()) {
        printf("Exception occurred after eval ocean model: %s\n", jl_string_ptr(jl_exception_occurred()));
    }
 


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
