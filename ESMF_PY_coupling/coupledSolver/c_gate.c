#include <stdio.h>
#include <mpi.h>
#include <Python.h>
#include <mpi4py/mpi4py.h>

extern "C" {

static PyObject *pPOMInst = NULL;

/*
MPI_Fint f_MPI_Comm_c2f(int *comm) {

    printf("[C code] received comm = %ld\n", *comm);
    MPI_Comm_f2c(*comm);
    printf("[C code] c2f called.\n");
    

    return MPI_Comm_c2f(*comm);
}
*/


void MARCOISCOOL_PYMODEL_INIT( int thread_id, int Fcomm) {

    printf("[C code] received Fcomm = %ld\n", Fcomm);
    MPI_Comm comm = MPI_Comm_f2c(Fcomm);

    // Step 2: Initialize the Python interpreter
    printf("[C Code] Initialize Python\n");
    Py_Initialize();

    printf("[C Code] Try to import_mpi4py();\n");
    import_mpi4py();
    printf("[C Code] Try to create a new stuff\n");
    PyObject *pCommon = PyMPIComm_New(comm);


    printf("[C Code] Add searching path\n");
    PyObject *sys_path = PySys_GetObject("path");
    PyObject *cwd = PyUnicode_FromString(".");
    PyList_Append(sys_path, cwd);
    Py_DECREF(cwd);

    printf("[C Code] Loading model...\n");
    PyObject *pName = PyUnicode_DecodeFSDefault("py_ocn_model");
    PyObject *pPOMModule = PyImport_Import(pName);
    Py_XDECREF(pName);


    printf("[C Code] Locating OceanModel...\n");
    PyObject *pClass = PyObject_GetAttrString(pPOMModule, "OceanModel");
    Py_XDECREF(pPOMModule);

    printf("[C Code] Calling constructor...\n");
    PyObject *pArgs  = PyTuple_Pack(2, PyLong_FromLong(thread_id), pCommon);
    pPOMInst = PyObject_CallObject(pClass, pArgs);
    Py_XDECREF(pClass);
    Py_XDECREF(pArgs);

    printf("[C Code] Printing report...\n");
    PyObject *pFuncReport = PyObject_GetAttrString(pPOMInst, "report");
    PyObject_CallObject(pFuncReport, NULL);
 
    Py_XDECREF(pFuncReport);
    
}

void MARCOISCOOL_PYMODEL_FINAL( int thread_id, int comm_id ) {

    printf("[C Code] Finalizing PYTHON...\n");
    Py_Finalize();

}

void MARCOISCOOL_PYMODEL_getDomainInfo( \
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
    if (Py_REFCNT(pPOMInst) <= 0) {
        printf("[C Code] Error: Ocean Model object is invalid (reference count is zero)\n");
    } else {
        
        printf("[C Code] Ocean Model object is valid.\n");

        PyObject *pFuncR = PyObject_GetAttrString(pPOMInst, "report");
        
        printf("[C Code] Gonna getDomainInfo.\n");
        PyObject *pFunc = PyObject_GetAttrString(pPOMInst, "getDomainInfo");
        
        printf("[C Code] Gonna call getDomainInfo.\n");
        PyObject *pReturnTuple = PyObject_CallObject(pFunc, NULL);
        
        printf("[C Code] Checking return tuple...\n");
        if (PyTuple_Check(pReturnTuple)) {
        
            printf("[C Code] Unpacking return tuple...\n");
            
            Py_ssize_t size = PyTuple_Size(pReturnTuple);

            if (size != 13) {
                PyErr_SetString(PyExc_TypeError, "Expected a tuple object with 13 integers.");
            }

            for (Py_ssize_t i = 0; i < size; ++i) {
            
                PyObject *item = PyTuple_GetItem(pReturnTuple, i);
                long value = PyLong_AsLong(item);

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

                Py_XDECREF(item);

            }
        } else {
            PyErr_SetString(PyExc_TypeError, "Expected a tuple object");
        } 

        Py_XDECREF(pReturnTuple);
        Py_XDECREF(pFunc);

        printf("[C Code] Exiting getDomainInfo\n");
    } 
}

}
