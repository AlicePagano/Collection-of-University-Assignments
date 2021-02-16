! This module contains the definition of common errors which may arise

MODULE ERROR_HANDLING_COMMON_ERRORS

    USE ERROR_HANDLING
    IMPLICIT NONE

    CONTAINS

        !> <b> Return an error if a variable is negative </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE error_neg_var(var,dim,ifail,info,method)
        !
        ! This subroutine takes as input a variable var of dimension dim
        ! and checks if the elements are negative. If at least one negative
        ! element is found, it returns an error.
        !
        SUBROUTINE error_neg_var(var,dim,ifail,info,method)
            character(len=*), intent(in), optional :: info
            character(len=*), intent(in), optional :: method
            type(error), intent(out), optional :: ifail
            integer :: dim
            integer, dimension(dim), intent(in) :: var
            integer :: ii

            DO ii=1,dim
                IF (var(ii)<0) THEN
                    call create_error(ifail,info,method)
                END IF
            END DO

        END SUBROUTINE error_neg_var


        !> <b> Return an error if a variable is negative </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE error_nequal_dim(N1,N2,ifail,info,method)
        !
        ! This subroutine takes as input two variables and check if they
        ! are not equal. In this case, an error is returned.
        !
        SUBROUTINE error_nequal_dim(N1,N2,ifail,info,method)
            character(len=*), intent(in), optional :: info
            character(len=*), intent(in), optional :: method
            type(error), intent(out), optional :: ifail
            integer :: N1, N2

            IF (N1.NE.N2) THEN
                    call create_error(ifail,info,method)
            END IF
        END SUBROUTINE error_nequal_dim


        !> <b> Return an error if a variable is negative </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE error_kind(ELEM,ELEM_KIND,ifail,info,method)
        !
        ! This subroutine takes as input two element kind and checks if they
        ! are not equal. In this case an error is returned.
        !
        SUBROUTINE error_kind(ELEM,ELEM_KIND,ifail,info,method)
            character(len=*), intent(in), optional :: info
            character(len=*), intent(in), optional :: method
            type(error), intent(out), optional :: ifail
            integer, intent(in) :: ELEM_KIND
            integer, intent(in) :: ELEM

            IF(ELEM.NE.ELEM_KIND) THEN
                    call create_error(ifail,info,method)
            END IF
        END SUBROUTINE error_kind


END MODULE ERROR_HANDLING_COMMON_ERRORS
