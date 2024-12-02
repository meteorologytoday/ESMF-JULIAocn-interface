#include <stdio.h>
#include <julia.h>
#include <mpi.h>
#include <string.h>

extern "C" {

jl_value_t *ocean_model;
jl_function_t *ocnRecvMsgFunc;

void testJuliaException(int ln, const char * msg){

    char new_msg[256] = "UNKNOWN";
    if (msg != NULL) {
        snprintf(new_msg, 256, "%s", msg);
    } 


    if (jl_exception_occurred()) {
        printf("[%s] Exception occurred at line %d [%s]: %s\n", __FILE__, ln, new_msg, jl_string_ptr(jl_exception_occurred()));
    }
        
}

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
    (void) jl_call1(func, comm_value);
    testJuliaException(__LINE__, "After pass MPI Communicator"); 
    
    printf("[C Code] Loading JlOceanModel.jl\n");
    (void) jl_eval_string("include(\"OceanModel/JlOceanModel.jl\")");
    printf("[C Code] Done loading JlOceanModel.jl\n");
    testJuliaException(__LINE__, "After loading JlOceanModel.jl");

    printf("[C Code] Initiating ocean model\n");
    snprintf(cmd, buffer_size, "model = Main.OceanModel.createOceanModel(\"model_config.toml\"; comm=COMM_ROOT)");
    printf("Going to eval: %s\n", cmd);
    (void) jl_eval_string(cmd);
    ocean_model = (jl_value_t *) jl_eval_string("model");
    testJuliaException(__LINE__, "Cannot obtain ocean model");

    // Setup some quick functions
    ocnRecvMsgFunc = jl_eval_string("OceanModel.receiveMessage!");
    testJuliaException(__LINE__, "Obtain OceanModel.receiveMessage!");
 
    printf("[C Code] End of initiation.\n");
}

void MARCOISCOOL_JLMODEL_sendInfo2Model( const char * msg ) {

    printf("[C Code] Gonna send message: \"%s\"\n", msg);
 

    jl_value_t *julia_str = jl_cstr_to_string(msg);

    (void) jl_call2(ocnRecvMsgFunc, (jl_value_t *) ocean_model, (jl_value_t*) julia_str);

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
    
    printf("[C Code] Getting domain info...\n");

    size_t param_len = 13;
    jl_value_t* array_type = jl_apply_array_type((jl_value_t*) jl_int64_type, 1);
    jl_array_t* param          = jl_alloc_array_1d(array_type, param_len);
    int64_t *paramData = jl_array_data(param, int64_t);
    for (size_t i = 0; i < jl_array_nrows(param); i++) {
        paramData[i] = 0;
    }

    jl_function_t *func = jl_eval_string("OceanModel.getDomainInfo!");
    (void) jl_call2(func, (jl_value_t *) ocean_model, (jl_value_t*) param);

    for(int i=0; i < jl_array_nrows(param); i+=1) {

        int64_t value = paramData[i];

        if (i == 0) {
            *sNx = value;
        } else if (i == 1) {
            *sNy = value;
        } else if (i == 2) {
            *OLx = value;
        } else if (i == 3) {
            *OLy = value;
        } else if (i == 4) {
            *nSx = value;
        } else if (i == 5) {
            *nSy = value;
        } else if (i == 6) {
            *nPx = value;
        } else if (i == 7) {
            *nPy = value;
        } else if (i == 8) {
            *Nx = value;
        } else if (i == 9) {
            *Ny = value;
        } else if (i == 10) {
            *Nr = value;
        } else if (i == 11) {
            *myXGlobalLo = value;
        } else if (i == 12) {
            *myYGlobalLo = value;
        }

    }

    printf("[C Code] Exiting getDomainInfo\n");

} 

} // extern C bracket
