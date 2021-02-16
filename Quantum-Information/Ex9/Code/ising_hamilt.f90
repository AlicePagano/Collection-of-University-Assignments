! This program compute the spectrum of an Ising Hamiltonian

! To compile: gfortran -o ising_hamilt ising_hamilt.f90 -L/usr/local/opt/lapack/lib -llapack
! To run (example): ./ising_hamilt 2 2.00 prova.txt 4

module ising_hamilt

    contains

        ! Compute tensor product between two generic matrices
        function tensor_product(Mat1,Mat2) result(Tens)

            integer :: ii, jj, kk, mm
            complex(8), dimension(:,:) :: Mat1, Mat2
            complex(8), dimension(:,:), allocatable :: Tens
            integer, dimension(2) :: N1, N2, N

            N1 = shape(Mat1)
            N2 = shape(Mat2)
            N(1) = N1(1)*N2(1)
            N(2) = N1(2)*N2(2)

            allocate(Tens(N(1),N(2)))

            do ii=1,N1(1),1
                do jj=1,N1(2),1
                    do kk=1,N2(1),1
                        do mm=1,N2(2),1
            		        Tens( (ii-1)*N2(1)+kk, (jj-1)*N2(2)+mm ) = Mat1(ii,jj)*Mat2(kk,mm)
                        end do
                    end do
                end do
            end do

        end function tensor_product


        ! Compute 2^N x 2^N identity matrix
        function identity(N) result(idMat)

            integer :: N, dim
            integer :: ii, jj
            complex(8), dimension(:,:), allocatable :: idMat

            dim = 2**N
            allocate(idMat(dim,dim))

            do ii=1,dim,1
                do jj=1,dim,1
            	    if (ii.EQ.jj) then
            		    idMat(ii,jj) = cmplx(1.0,0.0)
                    else
            		    idMat(ii,jj) = cmplx(0.0,0.0)
                    end if
                end do
            end do

        end function identity


        ! Compute non-interacting term of Ising Hamiltonian
        function H_non_int(N, sigmaz) result(H0)

            integer :: N, dim
            integer :: ii, jj, kk
            complex(8), dimension(:,:) :: sigmaz
            complex(8), dimension(:,:), allocatable :: H0
            complex(8), dimension(:,:), allocatable :: tmpMat

            dim = size(sigmaz,1)**N
            allocate(H0(dim,dim))
            allocate(tmpMat(dim,dim))

            H0 = cmplx(0.0,0.0)

            do ii=1,N,1
                tmpMat = tensor_product ( tensor_product( identity(ii-1), sigmaz) , identity(N-ii) )
                H0 = H0 + tmpMat
            end do

        end function H_non_int


        ! Compute interacting term of Ising Hamiltonian
        function H_int(N,sigmax) result(H1)

            integer :: N, dim
            integer :: ii, jj, kk
            complex(8), dimension(:,:) :: sigmax
            complex(8), dimension(:,:), allocatable :: H1
            complex(8), dimension(:,:), allocatable :: tmpMat

            dim = size(sigmax,1)**N
            allocate(H1(dim,dim))
            allocate(tmpMat(dim,dim))

            H1 = cmplx(0.0,0.0)

            do ii=1,N-1,1
                tmpMat = tensor_product(  tensor_product ( tensor_product( identity(ii-1), sigmax) , sigmax ), identity(N-ii-1) )
                H1 = H1 + tmpMat
            end do

        end function H_int


end module ising_hamilt




module file

    contains

        subroutine write_file(folder_name,file_name,lambda,eigen_val,klevels)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            real(8), intent(in) :: lambda
            real(8), dimension(:), intent(in) :: eigen_val
            integer, intent(in) :: klevels
            integer :: ii

            ! Create a folder
            folder_name = trim(folder_name)
            ! check if the folder exist
            inquire(file=trim(folder_name), exist=exist)
            if (.not.exist) then
                call system('mkdir '//folder_name)
            end if

            ! Create a file
            file_name = trim(file_name)
            ! Open file for writing
            inquire(file=trim(folder_name//file_name), exist=exist)
            if (exist) then
                open(file, file=trim(folder_name//file_name), status="old", position="append", action="write")
            else
                open(file, file=trim(folder_name//file_name), status="new", action="write")
            end if

            ! Write into file
            write(file, "(g0a)", ADVANCE="NO") lambda, char(9)

            do ii=1,klevels,1
                write(file, "(g0a)", ADVANCE="NO") eigen_val(ii), char(9)
            end do

            close(file)

        end subroutine write_file


        subroutine write_eigen(folder_name,file_name,eigen_val)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            real(8), dimension(:), intent(in) :: eigen_val
            integer :: ii

            ! Create a folder
            folder_name = trim(folder_name)
            ! check if the folder exist
            inquire(file=trim(folder_name), exist=exist)
            if (.not.exist) then
                call system('mkdir '//folder_name)
            end if

            ! Create a file
            file_name = trim(file_name)
            ! Open file for writing
            inquire(file=trim(folder_name//file_name), exist=exist)
            if (exist) then
                open(file, file=trim(folder_name//file_name), status="old", position="append", action="write")
            else
                open(file, file=trim(folder_name//file_name), status="new", action="write")
            end if

            do ii=1,size(eigen_val),1
                write(file, "(g0ag0)") ii,char(9),eigen_val(ii)
            end do

            close(file)

        end subroutine write_eigen

end module file




program demo

    use ising_hamilt
    use file
    IMPLICIT none

    ! variables
    integer(4) :: N_part, dim
    integer(4) :: klevels
    real(8) :: lambda
    complex(8), dimension(:,:), allocatable :: sigmaz, sigmax, H
    ! Input auxiliar variables
    CHARACTER(len=:), allocatable :: N_partchar, lamdachar, kchar
    integer :: arglen
    ! Diagonalization variables
    integer :: info
    complex(8), dimension(:), allocatable :: work, rwork
    real(8), dimension(:), allocatable :: eigen_val
    ! Timing variables
    real(4) :: t1, t2, dt
    ! file variables
    character(len=8) :: folder_name='results/'
    character(len=12) :: folder_name_new='results_new/'
    character(len=:), allocatable :: file_name
    character(len=:), allocatable :: file_name_new

    ! GET INPUT FROM COMMAND LINE

    ! Get number of particles N_part as input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: N_partchar)
    call GET_COMMAND_ARGUMENT(1, value=N_partchar)
    ! convert command line argument to integer (matrix size)
    read(N_partchar(:),'(i7)') N_part

    ! Get interaction strength lamda as input from command line
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: lamdachar)
    call GET_COMMAND_ARGUMENT(2, value=lamdachar)
    ! convert command line argument to integer (matrix size)
    read(lamdachar(:),'(f3.2)') lambda

    ! Get file name as input from command line
    call GET_COMMAND_ARGUMENT(3, length=arglen)
    allocate(character(arglen) :: file_name)
    call GET_COMMAND_ARGUMENT(3, value=file_name)

    ! Get k levels to save as input from command line
    call GET_COMMAND_ARGUMENT(4, length=arglen)
    allocate(character(arglen) :: kchar)
    call GET_COMMAND_ARGUMENT(4, value=kchar)
    read(kchar(:),'(i7)') klevels

    print *, 'Number of system particles:', N_part
    print *, 'Interaction strength:', lambda

    ! Allocate matrices
    allocate(sigmaz(2,2))
    allocate(sigmax(2,2))
    allocate(H(2**N_part,2**N_part))

    ! Define Pauli's matrices
    sigmaz = reshape( (/ cmplx(1.0,0.0) , cmplx(0.0,0.0)  , cmplx(0.0,0.0), cmplx(-1.0,0.0) /) , (/ 2,2 /) )
    sigmax = reshape( (/ cmplx(0.0,0.0) , cmplx(1.0,0.0)  , cmplx(1.0,0.0), cmplx( 0.0,0.0) /) , (/ 2,2 /) )

    ! Initilize Ising model Hamiltonian
    call cpu_time(t1)
    H = lambda*H_non_int(N_part,sigmaz) + H_int(N_part,sigmax)
    call cpu_time(t2)
    ! Compute elapsed time for hamiltonian initialization
    dt = t2 - t1
    print *, 'Time initialization:', dt

    ! Allocate auxiliar vector for hamiltonian diagonalization
    dim = 2**N_part
    allocate(eigen_val(dim))
    allocate(work(2*(dim-1)))

    if(dim>2) then
        allocate(rwork(3*(dim-2)))
    else
        allocate(rwork(1))
    end if

    ! Diagonalize Ising model hamiltonian
    call cpu_time(t1)
    CALL zheev('N','U', dim, H, dim, eigen_val, work, 2*dim, rwork, info)
    call cpu_time(t2)
    ! Compute elapsed time for hamiltonian initialization
    dt = t2 - t1
    print *, 'Time diagonalization:', dt
    !print *, 'Info diagonalization:', info

    call write_file(folder_name,file_name,lambda,eigen_val,klevels)

    allocate(character(len(file_name)+6) :: file_name_new)
    file_name_new = 'new_'//lamdachar//'_'//file_name
    call write_eigen(folder_name_new,file_name_new,eigen_val)

    deallocate(eigen_val,rwork,work)
    deallocate(H)

end program demo
