
#include "elem_datatype.F"

MODULE MATRIX_TYPE

    USE ERROR_HANDLING
    USE ERROR_HANDLING_COMMON_ERRORS
    IMPLICIT NONE

    integer, parameter, public :: ZM_ELEM_KIND = kind( (1d0,0d0) ) ! Complex in double precision
    integer, parameter, public :: DM_ELEM_KIND = kind( (1d0) )     ! Real in double precision

#if ELEM == D
    integer, parameter, public :: ELEM_KIND = DM_ELEM_KIND
#elif ELEM == Z
    integer, parameter, public :: ELEM_KIND = ZM_ELEM_KIND
#endif

    ! Specific element values
#if ELEM == D
    real(kind=DM_ELEM_KIND), parameter, public :: T_ELEM_ZERO = 0d0
    real(kind=DM_ELEM_KIND), parameter, public :: T_ELEM_ONE  = 1d0
#elif ELEM == Z
    complex(kind=ZM_ELEM_KIND), parameter, public :: T_ELEM_ZERO = (0d0, 0d0)
    complex(kind=ZM_ELEM_KIND), parameter, public :: T_ELEM_ONE  = (1d0, 0d0)
#endif


    !> <b> Error type </b>
    !
    ! ========== DOCUMENTATION ==========
    !
    ! Arguments:
    ! ==========
    !
    !> \verbatim
    !> \param N is an integer
    !> \param Elem
    !> \param Det
    !> \param Tr
    !> \endverbatim
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


        !> <b> User-defined function for matrix-matrix multiplication </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatMultbyRow(MServA,MServB,MServC)
        !
        ! This subroutine compute the matrix-matrix multiplication between matrix
        ! A and matrix B, returning matrix C. It access the matrix elements by rows (it is faster).
        ! It checks if the dimension are corrected both before and after the multiplication.
        ! It checks if the kind is the one wanted.
        !
        SUBROUTINE MatMultbyRow(MServA,MServB,MServC)
            IMPLICIT NONE
            TYPE(Matrix), INTENT(IN) :: MServA ! ii x kk
            TYPE(Matrix), INTENT(IN):: MServB  ! kk x jj
            TYPE(Matrix), INTENT(INOUT):: MServC ! ii x jj
            integer :: jj, kk, ii
            type(error) :: ifail

            call error_neg_var(MServA%N,2,ifail,'matrix dimension must be > 0','MatMult A')
            call error_neg_var(MServB%N,2,ifail,'matrix dimension must be > 0','MatMult B')
            call error_neg_var(MServC%N,2,ifail,'matrix dimension must be > 0','MatMult C')

            call error_nequal_dim(MServA%N(2),MServB%N(1),ifail,'dimension mismatch before matrix-matrix multiplication.','MatMult')

            !$acc data copyin(A,B) copyout(C)
            !$acc kernels
            do jj=1,MServB%N(2)
                do kk=1,MServA%N(2)
                    do ii=1,MServA%N(1)
                        MServC%Elem(ii,jj) = MServA%Elem(ii,kk) * MServB%Elem(kk,jj) + MServC%Elem(ii,jj)
                    end do
                end do
            end do
            !$acc end kernels
            !$acc end data

            call error_nequal_dim(MServA%N(1),MServC%N(1),ifail,'dimension mismatch after matrix-matrix multiplication.','MatMult')
            call error_nequal_dim(MServB%N(2),MServC%N(2),ifail,'dimension mismatch after matrix-matrix multiplication.','MatMult')
            call error_kind(kind(MServC%Elem),ELEM_KIND,ifail,'the kind of the matrix element &
                                                                after multiplication is changed.','MatMult')

            RETURN
        END SUBROUTINE MatMultbyRow


        !> <b> User-defined function for matrix-matrix multiplication </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatMultbyRow(MServA,MServB,MServC)
        !
        ! This subroutine compute the matrix-matrix multiplication between matrix
        ! A and matrix B, returning matrix C. It access the matrix elements by columns.
        ! It checks if the dimension are corrected both before and after the multiplication.
        ! It checks if the kind is the one wanted.
        !
        SUBROUTINE MatMultbyCol(MServA,MServB,MServC)
            IMPLICIT NONE
            TYPE(Matrix), INTENT(IN) :: MServA ! ii x kk
            TYPE(Matrix), INTENT(IN):: MServB  ! kk x jj
            TYPE(Matrix), INTENT(INOUT):: MServC ! ii x jj
            integer :: jj, kk, ii
            type(error) :: ifail

            call error_neg_var(MServA%N,2,ifail,'matrix dimension must be > 0','MatMult A')
            call error_neg_var(MServB%N,2,ifail,'matrix dimension must be > 0','MatMult B')
            call error_neg_var(MServC%N,2,ifail,'matrix dimension must be > 0','MatMult C')

            call error_nequal_dim(MServA%N(2),MServB%N(1),ifail,'dimension mismatch before matrix-matrix multiplication.','MatMult')

            do ii=1,MServA%N(1)
                do jj=1,MServB%N(2)
                    do kk=1,MServA%N(2)
                        MServC%Elem(ii,jj) = MServA%Elem(ii,kk) * MServB%Elem(kk,jj) + MServC%Elem(ii,jj)
                    end do
                end do
            end do

            call error_nequal_dim(MServA%N(1),MServC%N(1),ifail,'dimension mismatch after matrix-matrix multiplication.','MatMult')
            call error_nequal_dim(MServB%N(2),MServC%N(2),ifail,'dimension mismatch after matrix-matrix multiplication.','MatMult')
            call error_kind(kind(MServC%Elem),ELEM_KIND,ifail,'the kind of the matrix element &
                                                                after multiplication is changed.','MatMult')

            RETURN
        END SUBROUTINE MatMultbyCol


        !> <b> User-defined function for computing matrix adjoint </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatAdjoint(MServ)
        !
        ! This subroutine compute the adjoint of a matrix.
        !
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


        !> <b> User-defined function for computing matrix trace </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatTrace(MServ)
        !
        ! This subroutine compute the trace of a matrix.
        !
        SUBROUTINE MatTrace(MServ)
            IMPLICIT NONE
            TYPE(Matrix), INTENT(INOUT):: MServ
            integer :: i
            type(error) :: ifail

            MServ%Tr = T_ELEM_ZERO

            call error_nequal_dim(MServ%N(1),MServ%N(2),ifail,'trying to compute trace of a not square matrix','MatTrace')

            IF( MServ%N(1)==MServ%N(2) )  THEN
                do i = 1, MServ%N(1)
                    MServ%Tr = MServ%Tr + MServ%Elem(i,i)
                end do
            END IF

            RETURN
        END SUBROUTINE MatTrace


        !> <b> Initialize type(Matrix) variable </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatInit(MatServ, N, ctrl)
        !
        ! This subroutine initialize a type(matrix) variable. It first checks
        ! if the dimension given are > 0. Then, the initialization values depend
        ! on the 'ctrl' option. If:
        ! 'ctrl'=='N' : no initialization
        ! 'ctrl'=='Z' : initialize with zeros
        ! 'ctrl'=='O' : initialize with ones
        ! 'ctrl'=='R' : random initialization
        ! If a 'ctrl' illegal value is inserted, it returns an error.
        ! By default the matrix elements are initialized to zero.
        ! The trace and determinant are initialized to zero too.
        !
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
            type(error) :: ifail

            call error_neg_var(N,2,ifail,'Matrix dimension must be > 0','MatInit')

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
                        call create_error(ifail,'Illegal value for ctrl','MatInit')
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


        !> <b> Write type(Matrix) variable </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatWrite(MatServ,MatName)
        !
        ! This subroutine write all the type(matrix) arguments in a .txt file.
        !
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


        !> <b> Delete type(Matrix) variable </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE MatWrite(MatServ,MatName)
        !
        ! This subroutine delet a type(matrix) variable. If the matrix does not
        ! exist it returns an error.
        !
        SUBROUTINE MatDel(MatServ)
            IMPLICIT NONE
            TYPE(Matrix)  :: MatServ
            INTENT(INOUT) :: MatServ
            type(error) :: ifail

            IF(Allocated(MatServ%Elem))THEN
               MatServ%N=0
               DEALLOCATE(MatServ%Elem)
            ELSE
               call create_error(ifail,'trying to delete a non existent matrix!','MatDel')
            END IF
            RETURN
        END SUBROUTINE MatDel


END MODULE MATRIX_TYPE
