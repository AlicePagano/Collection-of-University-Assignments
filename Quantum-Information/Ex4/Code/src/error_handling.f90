! This module handle general errors and contains subroutines which stops the program in the presence of an error and prints its info

MODULE ERROR_HANDLING

    IMPLICIT NONE

    !> <b> Error type </b>
    !
    ! ========== DOCUMENTATION ==========
    !
    ! Arguments:
    ! ==========
    !
    !> \verbatim
    !> \param info is a string
    !> \param method is a string
    !> \endverbatim
    type, public :: error
        character(:), allocatable :: info
        character(:), allocatable :: method
    end type error


    CONTAINS

        !> <b> Create an error </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE create_error(ifail,info,method)
        !
        ! This subroutine call the error_constructor.
        !
        SUBROUTINE create_error(ifail,info,method)
            type(error), intent(out), optional :: ifail
            character(len=*), intent(in), optional :: info
            character(len=*), intent(in), optional :: method

            call error_constructor(ifail,info,method)
        END SUBROUTINE create_error


        !> <b> Construct an error </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE error_constructor(ifail,info,method)
        !
        ! This subroutine associate info and method to ifail variable.
        ! At the end, it stops the execution of the program if an error
        ! is found.
        !
        SUBROUTINE error_constructor(ifail,info,method)
            type(error), intent(out) :: ifail
            character(len=*), intent(in), optional :: info
            character(len=*), intent(in), optional :: method

            if( present(info) ) then
                if( len_trim(info) > 0 ) then
                    ifail%info = info
                end if
            end if

            if( present(method) ) then
                if( len_trim(method) > 0 ) then
                    ifail%method = method
                end if
            end if

            call error_write(ifail)
            print *, ''
            print *, achar(27)//"[1;91m"// "INTERRPUTING PROGRAM!"//achar(27)//"[0m"
            STOP
        END SUBROUTINE error_constructor


        !> <b> Write error info and method </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! SUBROUTINE error_write(ifail)
        !
        ! This subroutine write in a formatted way the info and method associated
        ! to the error.
        !
        SUBROUTINE error_write(ifail)
            type(error) :: ifail

            if( len_trim(ifail%info) > 0 ) then
                print *, achar(27)//"[1;91m"// "ERROR: "//achar(27)//"[0m", achar(27)//"[36m"// ifail%info //achar(27)//"[0m"
            else
                print *, achar(27)//"[1;91m"// "ERROR: "//achar(27)//"[0m", 'not specified.'
            end if

            if( len_trim(ifail%method) > 0 ) then
                print *, achar(27)//"[1;32m"// "METHOD: "//achar(27)//"[0m", achar(27)//"[36m"// ifail%method //achar(27)//"[0m"
            else
                print *, achar(27)//"[1;32m"// "METHOD: "//achar(27)//"[0m", 'not specified.'
            end if

        END SUBROUTINE error_write


END MODULE ERROR_HANDLING
