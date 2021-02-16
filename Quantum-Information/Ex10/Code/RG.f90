! This program compute the spectrum of an Ising Hamiltonian

! To compile: gfortran -o RG RG.f90 -L/usr/local/opt/lapack/lib -llapack
! To run (example): ./RG 2 prova.dat 100

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



module RG_algorithm

    use ising_hamilt

    contains

        ! Implement real-space Renormalization group algorithm
        subroutine real_space_RG(HN, N, A, B)

            complex(8), dimension(:,:) :: HN, A, B
            integer(4) :: N, dim
            complex(8), dimension(:,:), allocatable :: H2N, P, Pdag
            real(8), dimension(:), allocatable :: eig_val
            complex(8), dimension(:,:), allocatable :: eig_vec, A_int, B_int
            ! Loop variables
            integer :: ii

            ! Set dimension
            dim = 2**N

            allocate( H2N(dim**2,dim**2)     )
            !allocate( eig_vec(dim**2,dim**2) )
            !allocate( eig_val(dim**2)        )
            allocate( A_int(dim**2 ,dim**2)  )
            allocate( B_int(dim**2 ,dim**2)  )
            allocate( P(dim**2,dim)          )
            allocate( Pdag(dim,dim**2)       )

            ! Initialize H2N Hamiltonian
            H2N = tensor_product(HN, identity(N) ) + tensor_product(identity(N), HN ) + tensor_product(A,B)

            CALL diag_hamiltonian(H2N, eig_val, eig_vec)

            ! Initialize P and Pdag (adjoint)
            do ii = 1, dim, 1
            	P(:,ii) = eig_vec(:,ii)
            end do
            Pdag = transpose(conjg(P))

            ! Update H2N hamiltonian
            HN = matmul(matmul(Pdag, H2N),P)

            ! Update A_int and B_int
            A_int = tensor_product(A,identity(N))
            B_int = tensor_product(identity(N),B)
            A = matmul(matmul(Pdag,A_int),P)
            B = matmul(matmul(Pdag,B_int),P)

        end subroutine real_space_RG


        ! Diagonalize the Hamiltonian (leaving it unchanged) and store
        ! results in eig_val, eig_vac arrays
        subroutine diag_hamiltonian(H, eig_val, eig_vec)

            ! Diagonalization variables
            integer :: info
            complex(8), dimension(:), allocatable :: work, rwork
            real(8), dimension(:), allocatable :: eig_val
            complex(8), dimension(:,:), allocatable :: eig_vec
            ! Hamiltonian variables
            complex(8), dimension(:,:) :: H
            integer(4) :: dim
            ! Timing variables
            real(4) :: t1, t2, dt

            ! Take hamiltonian dimension
            dim  = size(H,1)

            allocate(eig_vec(dim,dim))
            allocate(eig_val(dim))

            eig_vec = H

            ! Allocation variables for diagonalization
            allocate(work(2*(dim-1)))

            if(dim>2) then
                allocate(rwork(3*(dim-2)))
            else
                allocate(rwork(1))
            end if

            ! Diagonalize hamiltonian
            call cpu_time(t1)
            CALL zheev('V','U', dim, eig_vec, dim, eig_val, work, 2*dim, rwork, info)
            call cpu_time(t2)
            ! Compute elapsed time for hamiltonian diagonalization
            dt = t2 - t1
            !print *, 'Time diagonalization:', dt

        end subroutine diag_hamiltonian


end module RG_algorithm


module file

    contains

        subroutine write_file(folder_name,file_name,lambda,state)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            real(8), dimension(:), intent(in) :: lambda, state
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
                open(file, file=trim(folder_name//file_name), status="old", position="rewind", action="write")
            else
                open(file, file=trim(folder_name//file_name), status="new", action="write")
            end if

            do ii=1,size(lambda),1
                write(file, "(g0ag0)") lambda(ii),char(9),state(ii)
            end do

            close(file)

        end subroutine write_file


end module file



program demo

    use ising_hamilt
    use RG_algorithm
    use file
    IMPLICIT none

    ! variables
    integer(4) :: N_part, dim
    integer(4) :: iter
    real(8), dimension(:), allocatable :: lambda, GS
    real(8) :: lam
    complex(8), dimension(:,:), allocatable :: sigmaz, sigmax, H, A, B
    ! Input auxiliar variables
    CHARACTER(len=:), allocatable :: N_partchar, iterchar
    integer :: arglen
    ! Timing variables
    real(4) :: t1, t2, dt
    ! Loop variables
    integer :: ii, jj
    ! Diagonalization variables
    real(8), dimension(:), allocatable :: eig_val
    complex(8), dimension(:,:), allocatable :: eig_vec
    ! file variables
    character(len=8) :: folder_name='results/'
    character(len=:), allocatable :: file_name
    character(len=:), allocatable :: file_name_new

    ! GET INPUT FROM COMMAND LINE

    ! Get number of particles N_part as input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: N_partchar)
    call GET_COMMAND_ARGUMENT(1, value=N_partchar)
    ! convert command line argument to integer (matrix size)
    read(N_partchar(:),'(i7)') N_part

    ! Get file name as input from command line
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: file_name)
    call GET_COMMAND_ARGUMENT(2, value=file_name)

    ! Get number of iterations as input from command line
    call GET_COMMAND_ARGUMENT(3, length=arglen)
    allocate(character(arglen) :: iterchar)
    call GET_COMMAND_ARGUMENT(3, value=iterchar)
    read(iterchar(:),'(i7)') iter

    print *, 'Number of system particles:', N_part
    print *, 'Number of iteration:', iter

    allocate(lambda(100))
    allocate(GS(100))

    ! Create lambda array
    do ii =1,size(lambda),1
		lambda(ii) = -5.0 + DBLE( 10.0*ii/size(lambda) )
		!print *, 'Lambda element:', lambda(ii)
	end do

    ! Allocate matrices
    allocate(sigmaz(2,2))
    allocate(sigmax(2,2))
    allocate(H(2**N_part,2**N_part))
    allocate(A(2**N_part,2**N_part))
    allocate(B(2**N_part,2**N_part))

    ! Define Pauli's matrices
    sigmaz = reshape( (/ cmplx(1.0,0.0) , cmplx(0.0,0.0)  , cmplx(0.0,0.0), cmplx(-1.0,0.0) /) , (/ 2,2 /) )
    sigmax = reshape( (/ cmplx(0.0,0.0) , cmplx(1.0,0.0)  , cmplx(1.0,0.0), cmplx( 0.0,0.0) /) , (/ 2,2 /) )


    ! Loop over the possible lambda
    do ii=1,size(lambda),1

        lam = lambda(ii)

        ! Initialization of system Hamiltonian and of operators
        H = lam*H_non_int(N_part,sigmaz) + H_int(N_part,sigmax)
        A = tensor_product(identity(N_part-1),sigmax)
        B = tensor_product(sigmax,identity(N_part-1))

		! Perform RG algorithm
		do jj=1,iter,1
            ! Divide per 2 each cycle to keep the numbers low
            H = H * 0.5
            A = 1/sqrt(2.) * A
            B = 1/sqrt(2.) * B
            ! Call RG algorithm
    	    CALL real_space_RG(H, N_part, A, B)
		end do

		! Diagonalize final hamiltonian
        call diag_hamiltonian(H, eig_val, eig_vec)
        ! Find GS energy density
		GS(ii) = eig_val(1)/ DBLE(N_part)

        deallocate(eig_val,eig_vec)

	end do

    call write_file(folder_name,file_name,lambda,GS)

end program demo
