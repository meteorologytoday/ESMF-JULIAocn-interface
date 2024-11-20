#include <stdio.h>
#include <mpi.h>
#include <Python.h>

int main(int argc, char *argv[]) {


    printf("Initializing MPI\n");
    // Step 1: Initialize MPI in C
    MPI_Init(&argc, &argv);


    int rank, size, comm_id;
    
    comm_id = 0;
    MPI_Comm C_comm = MPI_COMM_WORLD;

    MPI_Comm_rank(C_comm, &rank);
    MPI_Comm_size(C_comm, &size);
 

    // Step 2: Initialize the Python interpreter
    printf("Initialize Python\n");
    Py_Initialize();

    printf("Add searching path\n");
    PyObject *sys_path = PySys_GetObject("path");
    PyObject *cwd = PyUnicode_FromString(".");
    PyList_Append(sys_path, cwd);
    Py_DECREF(cwd);


    printf("Load model\n");
    PyObject *pName = PyUnicode_DecodeFSDefault("py_ocn_model");
    PyObject *pPOMModule = PyImport_Import(pName);
    Py_XDECREF(pName);


    PyObject *pClass = PyObject_GetAttrString(pPOMModule, "OceanModel");

    PyObject *pArgs  = PyTuple_Pack(2, PyLong_FromLong(rank), PyLong_FromLong(comm_id));
    PyObject *pPOMInst = PyObject_CallObject(pClass, pArgs);
    Py_XDECREF(pClass);
    Py_XDECREF(pArgs);

    printf("Calling report...\n"); 
    PyObject *pFuncReport = PyObject_GetAttrString(pPOMInst, "report");
    PyObject_CallObject(pFuncReport, NULL);



    // Step 6: Finalize the Python interpreter
    Py_Finalize();

    // Step 7: Finalize MPI in C
    MPI_Finalize();

    return 0;
}
