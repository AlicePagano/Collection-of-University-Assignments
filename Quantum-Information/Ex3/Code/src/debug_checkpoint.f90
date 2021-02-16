MODULE DEBUG_CHECKPOINT

    USE MATRIX_TYPE
    USE ERROR_HANDLING
    USE ERROR_HANDLING_COMMON_ERRORS
    IMPLICIT NONE

    CONTAINS

        !> <b> Create a checkpoint </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE do_checkpoint(debug,cp_name)
        !
        ! This subroutine create a checkpoint. The number and time associated to
        ! the checkpoint are printed.
        !
        SUBROUTINE do_checkpoint(debug,cp_name)
        	implicit none
        	logical, intent(in) :: debug
            character(len=*), intent(in), optional :: cp_name  ! (optional) checkpoint name
            integer(4) :: cp_number=1 ! sequential numbering of checkpoints
            real(8) :: time=0

            IF (debug) THEN
                IF(present(cp_name)) THEN
                    print *, achar(27)//"[1;34m"// "CHECKPOINT: "//achar(27)//"[0m", cp_name
                ELSE
                    print *, achar(27)//"[1;34m"// "CHECKPOINT: "//achar(27)//"[0m"
                END IF
                call cpu_time(time)
                print *, "Number: ", cp_number
                print *, "Time  : ", time
                cp_number = cp_number + 1
                print *, ''
            END IF
    	END SUBROUTINE do_checkpoint


        !> <b> General check of type(matrix) variable </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE check_matrix(debug,MServ,cp_name)
        !
        ! This subroutine takes as input a type(Matrix) variable. In particular,
        ! it checks if the dimension are not negative and print them. It checks
        ! if the kind is the one wanted and print it. If the matrix is a square
        ! one it print also the trace if already computed.
        !
        SUBROUTINE check_matrix(debug,MServ,cp_name)
            implicit none
            logical, intent(in) :: debug
            TYPE(Matrix), INTENT(IN):: MServ
            character(len=*), intent(in), optional :: cp_name  ! (optional) checkpoint name
            type(error) :: ifail

            IF (debug) THEN
                call do_checkpoint(debug,cp_name)
                call error_neg_var(MServ%N,2,ifail,'matrix dimension must be > 0')
                print *, "Matrix Dimension: ", MServ%N(1), 'x', MServ%N(2)

                call error_kind(kind(MServ%Elem),ELEM_KIND,ifail,'different type.')
                print *, "Element kind    : ", kind(MServ%Elem)

                IF ( MServ%N(1)==MServ%N(2) ) THEN
                    print *, "Trace           : ", MServ%Tr
                ELSE
                    print *, "Trace           : ", 'not square matrix.'
                END IF
                print *, ''
            END IF
        END SUBROUTINE check_matrix


END MODULE DEBUG_CHECKPOINT
