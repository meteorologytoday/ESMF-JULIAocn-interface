#include <stdio.h>
#include <julia.h>

extern "C" {

void MARCOISCOOL_addnums( int* a, int* b ) 
{
    int c = (*a) + (*b);  /* convert pointers to values, then add them */
    printf("sum of %i and %i is %i\n", (*a), (*b), c );



    jl_init();
    (void) jl_eval_string("include(\"myjulia.jl\")");
    (void) jl_eval_string("println(sqrt(20.0))");
    (void) jl_eval_string("println(\"Say something...\")");

    printf("Getting function\n");
    jl_function_t *add_numbers = jl_get_function(jl_main_module, "add_numbers");

    printf("Boxing values...\n");
    jl_value_t *x = jl_box_float32((float)(*a));
    jl_value_t *y = jl_box_float32((float)(*b));
    
    printf("Calling Julia function\n");
    jl_value_t *result  = jl_call2(add_numbers, x, y);

    printf("Unboxing Julia value\n");
    float_t unboxed_result = jl_unbox_float32(result);

    printf("Result returned from Julia : %f\n", unboxed_result);

    jl_module_t *myJuliaModule = (jl_module_t*)jl_eval_string("myJuliaModule");
    jl_function_t *divide_numbers = jl_get_function(myJuliaModule, "divide_numbers");
    jl_value_t *result2 = jl_call2(divide_numbers, x, y);
    
    printf("Unboxing another Julia value\n");
    float_t unboxed_result2 = jl_unbox_float32(result2);
    printf("Result returned from Julia : %f\n", unboxed_result2);


    //float32_t unboxed_result = jl_unbox_int32(result);


    jl_atexit_hook(0);

}

}

