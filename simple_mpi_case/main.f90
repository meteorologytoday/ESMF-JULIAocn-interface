program mpi_example

    use mpi
    USE, INTRINSIC :: ISO_C_BINDING
    implicit none

    INTERFACE
      INTEGER FUNCTION MARCOISCOOL_addnums(thread_id, comm) BIND(C, name="MARCOISCOOL_addnums")
        import :: C_INT
        INTEGER(C_INT) :: thread_id
        INTEGER(C_INT) :: comm
      END FUNCTION MARCOISCOOL_addnums
    END INTERFACE



    ! Declare variables
    integer :: ierr               ! Error code
    integer :: rank, size         ! Rank of the process and total number of processes
    integer :: tag, status(MPI_STATUS_SIZE)

    ! Initialize MPI
    call MPI_Init(ierr)

    ! Get the rank of the current process
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    ! Get the total number of processes
    call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

    ! Print a message from each process
    print *, "MPI_COMM_WORLD = ", MPI_COMM_WORLD
    print *, 'Hello from process ', rank, ' of ', size
    print *, "Calling function: MARCOISCOOL_addnums"
    print *, MARCOISCOOL_addnums(rank, MPI_COMM_WORLD)


    ! Finalize MPI
    call MPI_Finalize(ierr)
end program mpi_example
