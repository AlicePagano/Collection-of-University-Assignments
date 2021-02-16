! To compile: gfortran -cpp -DELEM=D -c -o "matrix_type.o" "matrix_type.f90"

#include "elem_datatype.F"

MODULE MATRIX_TYPE

    IMPLICIT NONE

    integer, parameter, public :: ZM_ELEM_KIND = kind( (1d0,0d0) ) ! Complex in double precision
    integer, parameter, public :: DM_ELEM_KIND = kind( (1d0) )     ! Real in double precision

    ! Specific element values
#if ELEM == D
    real(kind=DM_ELEM_KIND), parameter, public :: T_ELEM_ZERO = 0d0
    real(kind=DM_ELEM_KIND), parameter, public :: T_ELEM_ONE  = 1d0
#elif ELEM == Z
    complex(kind=ZM_ELEM_KIND), parameter, public :: T_ELEM_ZERO = (0d0, 0d0)
    complex(kind=ZM_ELEM_KIND), parameter, public :: T_ELEM_ONE  = (1d0, 0d0)
#endif


    TYPE Matrix
        INTEGER, dimension(2) :: N
        ELEM_TYPE, dimension(:,:), allocatable :: Elem
        ELEM_TYPE :: Det
        ELEM_TYPE :: Tr
    END TYPE Matrix


    INTERFACE Trace
        MODULE PROCEDURE MatTrace
    END INTERFACE

    INTERFACE OPERATOR(.Adj.)
        MODULE PROCEDURE MatAdjoint
    END INTERFACE


    CONTAINS ! here is the list of the functions included

        FUNCTION MatAdjoint(MServ)
            IMPLICIT NONE
            TYPE(Matrix):: MServ, MatAdjoint
            INTENT(IN)  :: MServ

            CALL MatInit(MatAdjoint, Mserv%n((/ 2, 1 /)), ctrl='N')
#if ELEM == D
            MatAdjoint%elem=Transpose(Mserv%elem)
            IF( MatAdjoint%N(1)==MatAdjoint%N(2) )  THEN
                MatAdjoint%Tr = MServ%Tr
            END IF
#elif ELEM == Z
            MatAdjoint%elem=Transpose(conjg(Mserv%elem))
            IF( MatAdjoint%N(1)==MatAdjoint%N(2) )  THEN
                MatAdjoint%Tr = conjg(MServ%Tr)
            END IF
#endif
            RETURN
        END FUNCTION MatAdjoint


        SUBROUTINE MatTrace(MServ)
            IMPLICIT NONE
            TYPE(Matrix), INTENT(INOUT):: MServ
            integer :: i

            MServ%Tr = T_ELEM_ZERO

            IF( MServ%N(1)==MServ%N(2) )  THEN
                do i = 1, MServ%N(1)
                    MServ%Tr = MServ%Tr + MServ%Elem(i,i)
                end do
            ELSE
                print *, 'Warning: not square matrix.'
            END IF
            RETURN
        END SUBROUTINE MatTrace


        SUBROUTINE MatInit(MatServ, N, ctrl)
            TYPE(Matrix), INTENT(INOUT)       :: MatServ
            INTEGER, DIMENSION(2), INTENT(IN) :: N
            CHARACTER, INTENT(IN), OPTIONAL   :: ctrl

            INTEGER :: seed_size
            integer,allocatable,dimension(:) :: seed

#if ELEM == Z
            INTEGER :: ii, jj
            real(DM_ELEM_KIND) :: Rand_Re, Rand_Im
#endif

            IF(Allocated(MatServ%Elem)) CALL MatDel(MatServ)

            ALLOCATE(MatServ%Elem(N(1),N(2)))
            MatServ%N=N

            IF(PRESENT(ctrl)) THEN
                ! Check all the cases
                SELECT CASE(ctrl)
                    CASE('N')
                        ! No initialization
                    CASE('Z')
                        ! Initialize with zeros
                        MatServ%Elem=T_ELEM_ZERO
                    CASE('O')
                        ! Initialize with ones
                        MatServ%Elem=T_ELEM_ONE
                    CASE('R')
                        ! Initialize with random numbers [0,1]
                        ! Set seed
                        call random_seed() ! initialize with system generated seed
#if ELEM == D
                        call random_number(MatServ%Elem)
#elif ELEM == Z
                        do jj = 1,MatServ%N(2)
                           do ii = 1,MatServ%N(1)
                               call random_number(Rand_Re)
                               call random_number(Rand_Im)
                               MatServ%Elem(ii,jj) = cmplx(Rand_Re,Rand_Im)
                           end do
                        end do
#endif
                    CASE DEFAULT
                        PRINT *, ctrl
                        STOP 'MatInit: Illegal value for ctrl'
                END SELECT
            ELSE
                ! Default case without any ctrl given
                MatServ%Elem=T_ELEM_ZERO
            END IF

            ! Initialize Trace and Determinant to zero
            MatServ%Tr  = T_ELEM_ZERO
            MatServ%Det = T_ELEM_ZERO

            RETURN
        END SUBROUTINE MatInit


        SUBROUTINE MatWrite(MatServ,MatName)
            IMPLICIT NONE
            TYPE(Matrix)  :: MatServ
            INTENT(INOUT) :: MatServ
            CHARACTER(len=*), INTENT(IN), OPTIONAL :: MatName
            CHARACTER(len=:), allocatable :: trimMatName

            integer :: ii, jj
            integer, parameter :: file = 11

            IF(PRESENT(MatName)) THEN
                ! open the file for writing
                trimMatName = trim(adjustl(MatName))
                open(file,file=trim('matrix_' // trimMatName // '.txt'),form='formatted')
                write(file,fmt="(A,/)") 'VARIABLE MATRIX TYPE '
                write(file,fmt="(A,A,/)") 'NAME: ', trimMatName
            ELSE
                ! open the file for writing
                open(file,file='matrix.txt',form='formatted')
                write(file,fmt="(A,/)") 'VARIABLE MATRIX TYPE '
            END IF

            write(file,fmt="(A,i10.1,A,i10.1,/)") 'Size: ', MatServ%N(1), ' x ', MatServ%N(2)

            write(file,fmt="(A,/)") 'Elements (row-by-row):'
            ! Write matrix row-by-row
            do ii=1,MatServ%N(1)
                  write (file,*) MatServ%Elem(ii,:)
            end do

            IF( MatServ%N(1)==MatServ%N(2) )  THEN
                write(file,fmt="(/,A)",ADVANCE="NO") 'Trace: '
                write(file,*) MatServ%Tr
            ELSE
                write(file,fmt="(/,A)") 'Trace: not square matrix.'
            END IF

            write(file,fmt="(/,A)",ADVANCE="NO") 'Determinant: '
            write(file,*) 'To be computed'

            close(file)
        END SUBROUTINE MatWrite


        SUBROUTINE MatDel(MatServ)
            IMPLICIT NONE
            TYPE(Matrix)  :: MatServ
            INTENT(INOUT) :: MatServ

            IF(Allocated(MatServ%Elem))THEN
               MatServ%N=0
               DEALLOCATE(MatServ%Elem)
            ELSE
               STOP 'MatDel: trying to delete a non existent matrix!'
            END IF
            RETURN
        END SUBROUTINE MatDel

END MODULE MATRIX_TYPE
