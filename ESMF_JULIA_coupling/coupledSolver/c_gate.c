#include <stdio.h>
#include <julia.h>
#include <mpi.h>
#include <string.h>
#include <ESMC.h>

extern "C" {

jl_value_t *ocean_model_driver;
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

void testESMCException(int rc, int ln, const char * msg){

    char new_msg[256] = "UNKNOWN";
    if (msg != NULL) {
        snprintf(new_msg, 256, "%s", msg);
    } 


    if (rc != ESMF_SUCCESS) {
        printf("[%s] Exception occurred at line %d [%s]: rc = %d\n", __FILE__, ln, new_msg, rc);
    }
        
}

void MARCOISCOOL_JLMODEL_REGISTER_VARIABLE_REAL8(
    const char * varname,
    void * ptr,
    int size
) {
    printf("Register variable `%s` (REAL8) with size %d\n", varname, size);
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
    (void) jl_eval_string("include(\"main_scripts/main00_loadModule.jl\")");


    printf("[C Code] Boxing the communicator...\n");
    // Convert the MPI communicator to a Julia value
    jl_value_t *comm_value = jl_box_int64((int64_t)comm);
    printf("[C Code] comm = %d\n", (int64_t)comm);
 
    // Call a Julia function
    printf("[C Code] Calling passMPICommunicator\n");

    jl_function_t *func = jl_get_function(jl_main_module, "passMPICommunicator");
    (void) jl_call1(func, comm_value);
    testJuliaException(__LINE__, "After pass MPI Communicator"); 
    
    printf("[C Code] Loading JlOceanModel.jl\n");
    (void) jl_eval_string("include(\"main_scripts/main01_loadModule.jl\")");
    printf("[C Code] Done loading JlOceanModel.jl\n");
    testJuliaException(__LINE__, "After loading JlOceanModel.jl");

    printf("[C Code] Initiating ocean model\n");
    (void) jl_eval_string("include(\"main_scripts/main02_init.jl\")");
    //snprintf(cmd, buffer_size, "model = Main.OceanModel.createOceanModel(\"model_config.toml\"; comm=COMM_ROOT)");
    //printf("Going to eval: %s\n", cmd);
    //(void) jl_eval_string(cmd);
    ocean_model_driver = (jl_value_t *) jl_eval_string("dr");
    //testJuliaException(__LINE__, "Cannot obtain ocean model");

    // Setup some quick functions
    //ocnRecvMsgFunc = jl_eval_string("OceanModel.receiveMessage!");
    //testJuliaException(__LINE__, "Obtain OceanModel.receiveMessage!");
 
    printf("[C Code] End of initiation.\n");
}

void MARCOISCOOL_JLMODEL_RUN(
    ESMC_State *importState_ptr,
    ESMC_State *exportState_ptr,
    ESMC_TimeInterval *timeStep
) {

    printf("[C Code] Run Model...\n");

    ESMC_Field field;
    int localDe = 0;
    int rc = 0;

    rc = ESMC_StatePrint(*importState_ptr);
    testESMCException(rc, __LINE__, "Print importState");

    rc = ESMC_StatePrint(*exportState_ptr);
    testESMCException(rc, __LINE__, "Print exportState");
 
    rc = ESMC_StateGetField(*exportState_ptr, "sst", &field);
    testESMCException(rc, __LINE__, "After get field of sst");
    ESMC_FieldGetPtr(field, localDe, &rc);
    testESMCException(rc, __LINE__, "After get ptr sst");

    rc = ESMC_StateGetField(*importState_ptr, "rsns", &field);
    ESMC_FieldGetPtr(field, localDe, &rc);


    (void) jl_eval_string("include(\"main_scripts/main03_run.jl\")");

}

void MARCOISCOOL_JLMODEL_sendInfo2Model( const char * msg ) {

    printf("[C Code] Gonna send message: \"%s\"\n", msg);
 

    jl_value_t *julia_str = jl_cstr_to_string(msg);

    (void) jl_call2(ocnRecvMsgFunc, (jl_value_t *) ocean_model_driver, (jl_value_t*) julia_str);

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
    jl_array_t* param      = jl_alloc_array_1d(array_type, param_len);
    int64_t *paramData = jl_array_data(param, int64_t);
    for (size_t i = 0; i < jl_array_nrows(param); i++) {
        paramData[i] = 0;
    }

    printf("[C Code] Getting DriverModule.getDomainInfo funtion...\n");
    jl_function_t *func = jl_eval_string("DriverModule.getDomainInfo");
    testJuliaException(__LINE__, "After getting DriverModule.getDomainInfo");

    printf("[C Code] Calling funtion...\n");
    (void) jl_call2(func, (jl_value_t *) ocean_model_driver, (jl_value_t*) param);
    //(void) jl_call1(func, (jl_value_t *) ocean_model_driver);
    testJuliaException(__LINE__, "After calling DriverModule.getDomainInfo");

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

void MARCOISCOOL_PRINTCALENDAR(
    ESMC_Time *ptr,
    const char *timestr
) {

    char calname[1024];
    ESMC_Time t = *ptr;
    //ESMC_I4 yy, h;
    ESMC_Calendar cal;
    ESMC_CalKind_Flag calkindflag;
    int timezone;

    printf("[C Code] Enter MARCOISCOOL_PRINTCALENDAR\n");

    ESMC_TimePrint(t);
    ESMC_TimeGet(t, NULL, NULL, &cal, &calkindflag, &timezone);
    printf("[C Code] timestr=%s\n", timestr);

    if (calkindflag == ESMC_CALKIND_GREGORIAN) {
        snprintf(calname,  sizeof(calname), "GREGORIAN");
    } else if (calkindflag == ESMC_CALKIND_JULIAN) {
        snprintf(calname,  sizeof(calname), "JULIAN");
    } else if (calkindflag == ESMC_CALKIND_JULIANDAY) {
        snprintf(calname,  sizeof(calname), "JULIANDAY");
    } else if (calkindflag == ESMC_CALKIND_MODJULIANDAY) {
        snprintf(calname,  sizeof(calname), "MODJULIANDAY");
    } else if (calkindflag == ESMC_CALKIND_NOLEAP) {
        snprintf(calname,  sizeof(calname), "NOLEAP");
    } else if (calkindflag == ESMC_CALKIND_360DAY) {
        snprintf(calname,  sizeof(calname), "360DAY");
    } else if (calkindflag == ESMC_CALKIND_CUSTOM) {
        snprintf(calname,  sizeof(calname), "CUSTOM");
    } else if (calkindflag == ESMC_CALKIND_NOCALENDAR) {
        snprintf(calname,  sizeof(calname), "NOCALENDAR");
    } else {
        snprintf(calname,  sizeof(calname), "UNKNOWN");
    }

    printf("[C Code] calname = %s\n", calname);

    printf("[C Code] Leave MARCOISCOOL_PRINTCALENDAR\n");
}



} // extern C bracket
