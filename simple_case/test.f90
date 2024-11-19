PROGRAM hello
        
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE

    INTERFACE
      INTEGER FUNCTION addnums(a, b) BIND(C)
        import :: C_INT
        INTEGER(C_INT) :: a, b
      END FUNCTION addnums
    END INTERFACE

    integer :: a,b,c
  
    print *, 'Hello, World!'
    a = 1
    b = 2
    c = addnums(a,b)

END PROGRAM hello
