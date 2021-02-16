program demo

    USE MATRIX_TYPE
    USE DEBUG_CHECKPOINT
    USE ERROR_HANDLING_COMMON_ERRORS
    IMPLICIT none

    logical :: DEBUG
    real(8) :: dt_cpu
    TYPE(Matrix) :: A, B, C
    INTEGER, dimension(2) :: N1, N2, N3
    type(error) :: ifail
    INTEGER :: row1, row2, col1, col2

    ! Enable -debug flag in the compiler
    character(len=:), allocatable :: debug_option
    integer(4) :: arglen
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: debug_option)
    call GET_COMMAND_ARGUMENT(1, value=debug_option)

    if (debug_option == "-debug") then
            DEBUG = .TRUE.
        else
            DEBUG = .FALSE.
    end if

    print *, "Starting the program..."
    print *, ""

    print *, "Enter row and column"
    print *, "Matrix A:"
    read (*,*) row1, col1
    print *, "Matrix B:"
    read (*,*) row2, col2

    N1 = (/row1,col1/)
    N2 = (/row2,col2/)
    N3 = (/N1(1),N2(2)/)

    ! Initialize matrix A, B, C
    CALL MatInit(A, N1, ctrl='R') ! Random initialization
    CALL MatInit(B, N2, ctrl='R') ! Random initialization
    CALL MatInit(C, N3, ctrl='O') ! Initialize with zeros

    ! Compute trace of matrix A and B
    !CALL Trace(A)
    !CALL Trace(B)

    ! Check matrix A and B
    call check_matrix(debug,A)
    call check_matrix(debug,B)

    ! Matrix-matrix multiplication with user defined function
    CALL MatMult(A,B,C)

    ! Check matrix C
    call check_matrix(debug,C)
    ! Compute trace of matrix C
    !CALL Trace(C)

    !call dgemm('n','n', A%N(1),A%N(1),A%N(1),1.,A%Elem,A%N(1),B%Elem,A%N(1),1.,C%Elem,A%N(1))
    !C%Elem = matmul(A%Elem,B%Elem)

    ! Delete matrix A, B and C
    CALL MatDel(A)
    CALL MatDel(B)
    CALL MatDel(C)

    print *, "...ending the program."

end program demo
