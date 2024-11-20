#include <stdio.h>
#include <Python.h>

extern "C" {

void MARCOISCOOL_PYMODEL_INIT( int* thread_id, int* comm_id ) {

    // Step 2: Initialize the Python interpreter
    Py_Initialize();
    /*
    // Step 3: Import mpi4py in Python
    PyRun_SimpleString("import mpi4py.MPI as MPI");

    // Step 4: Get rank and size from MPI in Python
    PyRun_SimpleString("rank = MPI.COMM_WORLD.Get_rank()");
    PyRun_SimpleString("size = MPI.COMM_WORLD.Get_size()");

    // Step 5: Print something from Python
    PyRun_SimpleString("print(f'Python: Rank {MPI.COMM_WORLD.Get_rank()} of {MPI.COMM_WORLD.Get_size()}')");
    */
    PyObject *pName = PyUnicode_DecodeFSDefault("py_ocn_model");
    PyObject *pPOMModule = PyImport_Import(pName);
    Py_XDECREF(pName);

    PyObject *pClass = PyObject_GetAttrString(pPOMModule, "OceanModel")

    PyObject *pArgs  = PyTuple_Pack(2, PyLong_FromLong(thread_id), PyLong_FromLong(comm_id));
    PyObject *pPOMInst = PyObject_CallObject(pClass, pArgs)
    Py_XDECREF(pClass);
    Py_XDECREF(pArgs);
    
    PyObject *pFuncReport = PyObject_GetAttrString(pModule, "report");
    PyObject_CallObject(pFuncReport, NULL)

}

void MARCOISCOOL_PYMODEL_FINAL() {

    Py_Finalize();

}


}

