#include "ESMC.h"
#include <stdio.h>

int main() {

    printf("Hello, World!\n");

    int rc, ierr;
    ierr = ESMC_Initialize(NULL, ESMC_ArgLast);
    printf("ierr = %d\n", ierr);

    ESMC_Calendar cal = ESMC_CalendarCreate(
       "MyCal",                      // in
       ESMC_CALKIND_GREGORIAN,
        &ierr
    );
    printf("Create calendar. ierr = %d\n", ierr);


    ESMC_Time t ;

    printf("%d\n", ESMC_TimePrint(t));
    
    ierr = ESMC_TimeSet(  
        &t,
        2001,
        15,
        cal,
        ESMC_CALKIND_GREGORIAN,
        0
    );
    printf("%d\n", ESMC_TimePrint(t));
    
    int minute;
    
    ierr = ESMC_TimeGet(t, NULL, NULL, NULL, NULL, &minute, NULL, NULL);


    return 0;
}
