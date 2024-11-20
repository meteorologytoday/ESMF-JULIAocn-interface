#include <stdio.h>
#include <mpi.h>
#include <Python.h>

int main(int argc, char *argv[]) {
    // Step 1: Initialize MPI in C
    MPI_Init(&argc, &argv);

    // Step 2: Initialize the Python interpreter
    Py_Initialize();

    // Step 3: Import mpi4py in Python
    PyRun_SimpleString("import mpi4py.MPI as MPI");

    // Step 4: Get rank and size from MPI in Python
    PyRun_SimpleString("rank = MPI.COMM_WORLD.Get_rank()");
    PyRun_SimpleString("size = MPI.COMM_WORLD.Get_size()");

    // Step 5: Print something from Python
    PyRun_SimpleString("print(f'Python: Rank {MPI.COMM_WORLD.Get_rank()} of {MPI.COMM_WORLD.Get_size()}')");

    // Step 6: Finalize the Python interpreter
    Py_Finalize();

    // Step 7: Finalize MPI in C
    MPI_Finalize();

    return 0;
}
