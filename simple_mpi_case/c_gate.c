#include <mpi.h>
#include <stdio.h>
#include <julia.h>

extern "C" {

MPI_Comm get_comm_from_int(int comm_id) {
    if (comm_id == 0) {
        return MPI_COMM_WORLD;
    } else if (comm_id == 1) {
        return MPI_COMM_SELF;
    } else {
        printf("Invalid communicator ID\n");
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    return MPI_COMM_NULL; // For safety
}


void MARCOISCOOL_addnums( int* thread_id, int* comm ) 
{
    //int c = (*a) + (*b);  /* convert pointers to values, then add them */
    //printf("sum of %i and %i is %i\n", (*a), (*b), c );

    //printf("[C code] Initializing MPI\n");
    //MPI_Init(NULL, NULL);

    int comm_id = *comm;
    
    printf("[C code] Received comm_id = %d\n", comm_id);
    
    MPI_Comm C_comm = get_comm_from_int(comm_id);

    int rank, size;
    
    MPI_Comm_rank(C_comm, &rank);
    MPI_Comm_size(C_comm, &size);
    

    printf("[C code] Rank %d out of %d processes in communicator ID %d\n", rank, size, comm_id);

    jl_init();


    (void) jl_eval_string("using MPI");
    //(void) jl_eval_string("comm = MPI.Comm(unbox(Int32, boxed_comm))");
    //(void) jl_eval_string("println(\"Do this from C\");");
    //(void) jl_eval_string("println(string(MPI.Comm_rank(comm)))");

    
    //(void) jl_eval_string("println(\"!!!!!!!!!!!!!!!! MPI comm = \", comm);");
    //(void) jl_eval_string("println(\"My RANK = \", MPI.Comm_rank(comm));");
    (void) jl_eval_string("include(\"myjulia.jl\")");


    //printf("Getting function\n");
    jl_function_t *add_numbers = jl_get_function(jl_main_module, "add_numbers");

    printf("Calling Julia function\n");
    jl_value_t *boxed_thread_id = jl_box_int32((int)(*thread_id)); 
    jl_value_t *boxed_comm      = jl_box_int32((int)(*comm)) ;
    jl_value_t *result  = jl_call2(add_numbers, boxed_thread_id, boxed_comm);
    
    // Handle exception
    if (jl_exception_occurred()) {
        jl_value_t *ex = jl_exception_occurred();
        jl_exception_clear();  // Clear the exception so that we can continue
        jl_function_t *jl_string_func = jl_get_function(jl_base_module, "string");
        jl_value_t *str = jl_call1(jl_string_func, ex);  // Convert the exception to a string
        const char *errmsg = jl_string_ptr(str);
        printf("Error occurred: %s\n", errmsg);
    } else {
        printf("Julia code executed successfully.\n");
    }


    jl_atexit_hook(0);

}

}

