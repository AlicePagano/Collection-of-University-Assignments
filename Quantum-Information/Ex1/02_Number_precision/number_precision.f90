! To compile: gfortran number_precision.f90 -fno-range-check -o prova.x
! To run: ./prova.x
program number_precision

    implicit none

    integer(2) :: int2
    integer(4) :: int4
    real(4)    :: real4
    real(8)    :: real8

    integer(2), parameter :: int2_number   = 2000000
    integer(4), parameter :: int4_number   = 2000000
    real(4), parameter    :: single_pi     = 4.D0*DATAN(1.0d0)
    real(8), parameter    :: double_pi     = 4.D0*DATAN(1.0d0)
    real(4), parameter    :: single_number = sqrt(2.0e0)*1.e21
    real(8), parameter    :: double_number = sqrt(2.0d0)*1.d21

    print *, "Starting the program..."
    print *
    ! INTEGER VARIABLES
    print *, "INTEGER number:"
    print *
    ! number of bytes occupied in memory
    print *, "KIND = ", kind(int2)
    ! max value
    print *, "HUGE = ", huge(int2)
    print *
    print *, "KIND = ", kind(int4)
    ! max value
    print *, "HUGE = ", huge(int4), NEW_LINE(''), NEW_LINE('')

    ! REAL VARIABLES in single precision
    print *, "REAL number in SINGLE PRECISION:"
    print *
    ! number of bytes occupied in memory
    print *, "KIND = ",kind(real4)
    ! max value
    print *, "HUGE = ", huge(real4)
    ! number of significant digit and range
    print *, "PRECISION = ", precision(real4)
    print *, "RANGE = ", range(real4), NEW_LINE(''), NEW_LINE('')

    ! REAL VARIABLES in double precision
    print *, "REAL number in DOUBLE PRECISION"
    ! number of bytes occupied in memory
    print *, "KIND = ",kind(real8)
    ! max value
    print *, "HUGE = ", huge(real8)
    ! number of significant digit and range
    print *, "PRECISION =", precision(real8)
    print *, "RANGE = ", range(real8), NEW_LINE(''), NEW_LINE('')


    print *, "Let us print some examples..."
    print *
    print *, "Sum between integer(2) numbers:", int2_number + 1
    print *, "Sum between integer(4) numbers:", int4_number + 1
    print *, "Sum between real numbers in SINGLE PRECISION:             ", (single_pi*1.e32) + single_number
    print *, "Sum between the same real numbers but in DOUBLE PRECISION:", (double_pi*1.d32) + double_number

    print *
    print *, "...ending the program."

end program number_precision
