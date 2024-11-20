#include <stdio.h>
#include <Python.h>

extern "C" {

void MARCOISCOOL_PYMODEL_INIT( int* thread_id, int* comm_id ) {


    // Step 2: Initialize the Python interpreter
    printf("Initialize Python\n");
    Py_Initialize();

    printf("Add searching path\n");
    PyObject *sys_path = PySys_GetObject("path");
    PyObject *cwd = PyUnicode_FromString(".");
    PyList_Append(sys_path, cwd);
    Py_DECREF(cwd);

    printf("Loading model...\n");
    PyObject *pName = PyUnicode_DecodeFSDefault("py_ocn_model");
    PyObject *pPOMModule = PyImport_Import(pName);
    Py_XDECREF(pName);


    printf("Locating OceanModel...\n");
    PyObject *pClass = PyObject_GetAttrString(pPOMModule, "OceanModel");
    

    printf("Calling constructor...\n");
    PyObject *pArgs  = PyTuple_Pack(2, PyLong_FromLong(*thread_id), PyLong_FromLong(*comm_id));
    PyObject *pPOMInst = PyObject_CallObject(pClass, pArgs);
    Py_XDECREF(pClass);
    Py_XDECREF(pArgs);

    printf("Printing report...\n");
    PyObject *pFuncReport = PyObject_GetAttrString(pPOMInst, "report");
    PyObject_CallObject(pFuncReport, NULL);

}

void MARCOISCOOL_PYMODEL_FINAL() {

    Py_Finalize();

}


}

