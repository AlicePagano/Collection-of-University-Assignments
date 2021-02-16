! To compile: gfortran "matrix_type.o" "test.f90"

program demo

    USE MATRIX_TYPE
    implicit none

    TYPE(Matrix) :: A, B
    INTEGER, dimension(2) :: N
    INTEGER :: row, column

    print *, "Enter row and column:"
    read (*,*) row, column

    N = (/row,column/)

    print *, "Starting the program..."

    CALL MatInit(A, N, ctrl='R')

    CALL Trace(A)

    B = .Adj.A ! or MatAdjoint(A)

    CALL MatWrite(A,'A')
    CALL MatWrite(B,'A_adjoint')

    CALL MatDel(A)
    CALL MatDel(B)

    print *, "...ending the program."

end program demo
