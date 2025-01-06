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

/*
void MARCOISCOOL_JLMODEL_GETPUT_VARIABLE_REAL8(
    const char * varname,
    double * arr,
    int arr_size,
    int direction
) {

    
    printf("[C Code] Creating array in Julia... arr_size = %d\n", arr_size);
    jl_value_t *varname_julia_str = jl_cstr_to_string(varname);
    jl_function_t *func = jl_eval_string("createArray");
    jl_value_t *arr_size_jl = jl_box_int64(arr_size);
    jl_value_t *new_arr = jl_call2(func, varname_julia_str, arr_size_jl);
    double *new_arr_ptr = jl_array_data(new_arr, double);
    
    if (direction==0) { // CPL => COMP
        for (int i = 0 ; i < arr_size ; ++i) {
            new_arr_ptr[i] = arr[i];
        }
    } else if (direction==1) { // COMP => CPL
        for (int i = 0 ; i < arr_size ; ++i) {
            arr[i] = new_arr_ptr[i];
        }
    } else {
        printf("ERROR: Unknown direction = %d\n", direction);
    }
}
*/

double* MARCOISCOOL_JLMODEL_GET_VARIABLE_REAL8(
    const char * varname,
    int arr_size,
    int direction
) {

    // The code here should be something like getting
    // driver.x2c[varname] or driver.c2x[varname]
    printf("[C Code] Creating array in Julia... arr_size = %d\n", arr_size);
    jl_value_t *varname_julia_str = jl_cstr_to_string(varname);
    jl_function_t *func = jl_eval_string("createArray");
    jl_value_t *arr_size_jl = jl_box_int64(arr_size);
    jl_value_t *new_arr = jl_call2(func, varname_julia_str, arr_size_jl);
    double *new_arr_ptr = jl_array_data(new_arr, double);
    
    return new_arr_ptr;

}

void MARCOISCOOL_JLMODEL_REGISTER_TIME(
    ESMC_Time *caltime,
    const char *starttimestr,
    const char *stoptimestr,
    int32_t timeinterval_s
) {
    
    printf("[C Code] Enter MARCOISCOOL_JLMODEL_REGISTER_TIME\n");

    char caltype[1024];
    //ESMC_I4 yy, h;
    ESMC_Calendar cal;
    ESMC_CalKind_Flag calkindflag;
    int timezone;

    ESMC_I8 timeinterval_s8;

    //ESMC_TimePrint(t);
    ESMC_TimeGet(*caltime, NULL, NULL, &cal, &calkindflag, &timezone);
    printf("[C Code] starttimestr=%s\n", starttimestr);
    printf("[C Code] stoptimestr=%s\n", stoptimestr);
    
    //ESMC_TimeIntervalGet(*timeinterval, &timeinterval_s8, NULL);
    //printf("[C Code] timeinterval seconds = %d\n", (int) timeinterval_s8);

    if (calkindflag == ESMC_CALKIND_GREGORIAN) {
        snprintf(caltype,  sizeof(caltype), "GREGORIAN");
    } else if (calkindflag == ESMC_CALKIND_JULIAN) {
        snprintf(caltype,  sizeof(caltype), "JULIAN");
    } else if (calkindflag == ESMC_CALKIND_JULIANDAY) {
        snprintf(caltype,  sizeof(caltype), "JULIANDAY");
    } else if (calkindflag == ESMC_CALKIND_MODJULIANDAY) {
        snprintf(caltype,  sizeof(caltype), "MODJULIANDAY");
    } else if (calkindflag == ESMC_CALKIND_NOLEAP) {
        snprintf(caltype,  sizeof(caltype), "NOLEAP");
    } else if (calkindflag == ESMC_CALKIND_360DAY) {
        snprintf(caltype,  sizeof(caltype), "360DAY");
    } else if (calkindflag == ESMC_CALKIND_CUSTOM) {
        snprintf(caltype,  sizeof(caltype), "CUSTOM");
    } else if (calkindflag == ESMC_CALKIND_NOCALENDAR) {
        snprintf(caltype,  sizeof(caltype), "NOCALENDAR");
    } else {
        snprintf(caltype,  sizeof(caltype), "UNKNOWN");
    }

    printf("[C Code] caltype = %s\n", caltype);
    
    jl_function_t *func = jl_eval_string("setModelTimeInformation!");
    testJuliaException(__LINE__, "After getting function setModelTimeInformation!");

    jl_value_t *caltype_julia_str = jl_cstr_to_string(caltype);
    jl_value_t *starttimestr_julia_str = jl_cstr_to_string(starttimestr);
    jl_value_t *stoptimestr_julia_str = jl_cstr_to_string(stoptimestr);
    jl_value_t *timeinterval_s_julia_int = jl_box_int32(timeinterval_s);

    jl_value_t* args[5];
    args[0] = ocean_model_driver;
    args[1] = caltype_julia_str;
    args[2] = starttimestr_julia_str;
    args[3] = stoptimestr_julia_str;
    args[4] = timeinterval_s_julia_int;

    (void) jl_call(func, args, 5);
    testJuliaException(__LINE__, "After Calling setModelTimeInformation!");
 
    printf("[C Code] Leave MARCOISCOOL_JLMODEL_REGISTER_TIME\n");
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
    
    printf("[C Code] Loading Julia Ocean Model... \n");
    (void) jl_eval_string("include(\"jlocnmodel_code/main_scripts/main01_loadModule.jl\")");

    printf("[C Code] Loading Julia misc modules...\n");
    (void) jl_eval_string("include(\"jlocnmodel_code/main_scripts/main02_loadMiscModule.jl\")");

    printf("[C Code] Boxing the communicator...\n");
    // Convert the MPI communicator to a Julia value
    jl_value_t *comm_value = jl_box_int64((int64_t)comm);
    printf("[C Code] comm = %d\n", (int64_t)comm);
 
    // Call a Julia function
    printf("[C Code] Calling passMPICommunicator\n");

    jl_function_t *func = jl_get_function(jl_main_module, "passMPICommunicator");
    (void) jl_call1(func, comm_value);
    testJuliaException(__LINE__, "After pass MPI Communicator"); 
    
    printf("[C Code] Done loading JlOceanModel.jl\n");
    testJuliaException(__LINE__, "After loading JlOceanModel.jl");

    printf("[C Code] Create Driver\n");
    (void) jl_eval_string("include(\"jlocnmodel_code/main_scripts/main03_createDriver.jl\")");
    testJuliaException(__LINE__, "Cannot create Driver");
 
    ocean_model_driver = (jl_value_t *) jl_eval_string("dr");
    testJuliaException(__LINE__, "Cannot obtain model driver");
   
    printf("[C Code] Initiating ocean model\n");
    (void) jl_eval_string("include(\"jlocnmodel_code/main_scripts/main04_init.jl\")");
    testJuliaException(__LINE__, "Cannot initiate ocean model");
    
    printf("[C Code] End of initiation.\n");
}

void MARCOISCOOL_JLMODEL_RUN(
    ESMC_State *importState_ptr,
    ESMC_State *exportState_ptr,
    ESMC_TimeInterval *timeStep
) {

    printf("[C Code] Run Model...\n");
    (void) jl_eval_string("printXYZ()");

    /*
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
    */

    (void) jl_eval_string("include(\"jlocnmodel_code/main_scripts/main03_run.jl\")");

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

    printf("[C Code] Calling DriverModule.getDomainInfo funtion...\n");
    (void) jl_call2(func, (jl_value_t *) ocean_model_driver, (jl_value_t*) param);
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

    char caltype[1024];
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
        snprintf(caltype,  sizeof(caltype), "GREGORIAN");
    } else if (calkindflag == ESMC_CALKIND_JULIAN) {
        snprintf(caltype,  sizeof(caltype), "JULIAN");
    } else if (calkindflag == ESMC_CALKIND_JULIANDAY) {
        snprintf(caltype,  sizeof(caltype), "JULIANDAY");
    } else if (calkindflag == ESMC_CALKIND_MODJULIANDAY) {
        snprintf(caltype,  sizeof(caltype), "MODJULIANDAY");
    } else if (calkindflag == ESMC_CALKIND_NOLEAP) {
        snprintf(caltype,  sizeof(caltype), "NOLEAP");
    } else if (calkindflag == ESMC_CALKIND_360DAY) {
        snprintf(caltype,  sizeof(caltype), "360DAY");
    } else if (calkindflag == ESMC_CALKIND_CUSTOM) {
        snprintf(caltype,  sizeof(caltype), "CUSTOM");
    } else if (calkindflag == ESMC_CALKIND_NOCALENDAR) {
        snprintf(caltype,  sizeof(caltype), "NOCALENDAR");
    } else {
        snprintf(caltype,  sizeof(caltype), "UNKNOWN");
    }

    printf("[C Code] caltype = %s\n", caltype);

    printf("[C Code] Leave MARCOISCOOL_PRINTCALENDAR\n");
}



} // extern C bracket
