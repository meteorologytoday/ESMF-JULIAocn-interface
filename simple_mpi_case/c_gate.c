#include <stdio.h>
#include <julia.h>

extern "C" {

void MARCOISCOOL_addnums( int* thread_id, int* comm ) 
{
    //int c = (*a) + (*b);  /* convert pointers to values, then add them */
    //printf("sum of %i and %i is %i\n", (*a), (*b), c );



    jl_init();
    (void) jl_eval_string("include(\"myjulia.jl\")");
    (void) jl_eval_string("println(\"Say something...\")");

    printf("Getting function\n");
    jl_function_t *add_numbers = jl_get_function(jl_main_module, "add_numbers");
    //jl_module_t *jl_base_module = jl_get_module(jl_main_module, "base");

    printf("Boxing values...\n");
    jl_value_t *boxed_thread_id = jl_box_int64((int)(*thread_id)); 
    jl_value_t *boxed_comm      = jl_box_int64((int)(*comm)) ;
    printf("Calling Julia function\n");
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

