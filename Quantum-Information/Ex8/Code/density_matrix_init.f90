! This program initialize a pure state of a quantum system

! To compile: gfortran -o density_matrix density_matrix.f90

! To run (example): ./density_matrix 2 3

module quantum_state

    contains

        ! Initialize quantum pure state
        subroutine state_init(N,D,sep,state)

            implicit none
            integer, intent(in) :: N, D
            logical, intent(in) :: sep
            integer(4) :: dim
            complex(8), dimension(:), allocatable, intent(out) :: state
            real(8) :: rand_re, rand_im
            integer :: ii
            real(8) :: norm
            ! Check if separable or not separable state
            if(sep .eqv. .TRUE.) then
                dim = N * D
                print *, 'Separable state'
            else if(sep .eqv. .FALSE.) then
                dim = D**N
                print *, 'Non-separable state'
            end if

            print *, 'N  :', N
            print *, 'D  :', D
            print *, 'dim:', dim
            print *, ''

            allocate(state(dim))
            state = 0

            call random_seed() ! initialize with system generated seed

            do ii = 1,dim
                call random_number(rand_re)
                call random_number(rand_im)
                state(ii) = cmplx(2*rand_re-1,2*rand_im-1)
            end do

            ! Normalization of the coefficients
            norm = sum( state(:)*conjg(state(:)) )
            state(:) = state(:)/sqrt(norm)

        end subroutine state_init


        ! Compute density matrix of a pure state of N
        ! subsystems, each one belonging to a D dimensional
        ! Hilbert space
        function pure_density_matrix(state) result(densMat)

            implicit none
            complex(8), dimension(:) :: state
            complex(8), dimension(:,:), allocatable :: densMat
            complex(8), dimension(:,:), allocatable :: bra, ket
            integer(4) :: dim

            dim = size(state)

            allocate(densMat(dim,dim), bra(1,dim), ket(dim,1))

            ket(:,1) = state
            bra(1,:) = conjg(state)

            densMat = matmul(ket,bra)

            return
        end function pure_density_matrix


        ! Reduced density matrix: trace over system K
        function reduced_density_matrix(densMat,N,D,K) result(red_densMat)

            complex(8), dimension(:,:) :: densMat
            complex(8), dimension(:,:), allocatable :: red_densMat
            integer(4) :: N, D, dim
            complex(8) :: Tr
            integer(4) :: ii1, ii2, jj1, jj2, kk
            integer(4) :: K
            integer(4) :: ind_row, ind_col, ind_red_row, ind_red_col

            dim = size(densMat,1)

            allocate(red_densMat(D**(N-1),D**(N-1)))

            Tr = COMPLEX(0.0d0,0.0d0)

            do ii1=1,D**(K-1)
                do ii2=1,(D**(N-K))
                    do jj1=1,D**(K-1)
                        do jj2=1,(D**(N-K))
                            Tr = COMPLEX(0.0d0,0.0d0)
                            do kk=1,D
                            ind_row = (ii1-1) + 1 + ((kk-1) + (ii2-1)*D)*D**(K-1)
                            ind_col = (jj1-1) + 1 + ((kk-1) + (jj2-1)*D)*D**(K-1)
                            Tr = Tr + densMat(ind_row,ind_col)
                            end do
                            ! compute reduced density matrix
                            ind_red_row = (ii1-1) + 1 + (ii2-1)*D**(K-1)
                            ind_red_col = (jj1-1) + 1 + (jj2-1)*D**(K-1)
                            red_densMat(ind_red_row, ind_red_col) = Tr
                        end do
                    end do
                end do
            end do

        end function reduced_density_matrix

end module quantum_state


module file

    contains

        subroutine write_time(folder_name,file_name,N,D,dt)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            integer, intent(in) :: N, D
            real(4) :: dt
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
            ! open file for writing
            inquire(file=trim(folder_name//file_name), exist=exist)
            if (exist) then
              open(file, file=trim(folder_name//file_name), status="old", position="append", action="write")
            else
              open(file, file=trim(folder_name//file_name), status="new", action="write")
              write(file,*) 'N ', 'D ', 'dt'
            end if

            ! Write matrix row-by-row (with position column)
            write(file,*) N, D, dt

            close(file)

        end subroutine write_time


end module file



program demo

    use quantum_state
    use file
    IMPLICIT none

    ! variables
    integer(4) :: N, D
    logical :: sep
    CHARACTER(len=:), allocatable :: Nchar, Dchar, sep_string
    integer :: arglen
    complex(8), dimension(:), allocatable :: state
    complex(8), dimension(:,:), allocatable :: densMat, red_densMat1, red_densMat2
    integer(4) :: ii
    integer(4) :: K
    real(4) :: t1, t2, dt
    ! file variables
    character(len=8) :: folder_name
    character(len=:), allocatable :: file_name


    ! Get system dimension N as input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: Nchar)
    call GET_COMMAND_ARGUMENT(1, value=Nchar)
    ! convert command line argument to integer (matrix size)
    read(Nchar(:),'(i7)') N

    ! Get single vector space dimension D as input from command line
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: Dchar)
    call GET_COMMAND_ARGUMENT(2, value=Dchar)
    ! convert command line argument to integer (matrix size)
    read(Dchar(:),'(i5)') D

    ! Get single vector space dimension D as input from command line
    call GET_COMMAND_ARGUMENT(3, length=arglen)
    allocate(character(arglen) :: sep_string)
    call GET_COMMAND_ARGUMENT(3, value=sep_string)

    ! Get single vector space dimension D as input from command line
    call GET_COMMAND_ARGUMENT(4, length=arglen)
    allocate(character(arglen) :: file_name)
    call GET_COMMAND_ARGUMENT(4, value=file_name)

    ! Non separable state
    sep = .FALSE.

    if (sep_string=='T') then
        sep = .TRUE.
    else if (sep_string=='F') then
        sep = .FALSE.
    end if

    call cpu_time(t1)
    call state_init(N,D,sep,state)
    call cpu_time(t2)

    ! Compute elapsed time for density matrix computation
    dt = t2 - t1

    folder_name = 'results/'
    call write_time(folder_name,file_name,N,D,dt)

    !densMat = pure_density_matrix(state)

    ! Write matrix row-by-row
    !print *, 'Density Matrix:'
    !do ii=1,size(densMat,1)
    !      write(*, "(*('('sf9.6xspf9.6'i)':x))") densMat(ii,:)
    !end do
    !print *, ''

    !! Reduced matrix rho_1 = Tr_2 rho
    !K = 2
    !red_densMat1 = reduced_density_matrix(densMat,N,D,K)

    !print *, 'Reduced density Matrix 1:'
    !do ii=1,size(red_densMat1,1)
    !      write(*, "(*('('sf9.6xspf9.6'i)':x))") red_densMat1(ii,:)
    !end do
    !print *, ''

    !! Reduced matrix rho_2 = Tr_1 rho
    !K = 1
    !red_densMat2 = reduced_density_matrix(densMat,N,D,K)

    !print *, 'Reduced density Matrix 2:'
    !do ii=1,size(red_densMat2,1)
    !      write(*, "(*('('sf9.6xspf9.6'i)':x))") red_densMat2(ii,:)
    !end do
    !print *, ''

    !do ii=1,size(red_densMat2,1)
    !      write(*, "(*('('sf9.6xspf9.6'i)':x))") red_densMat2(ii,:)
    !end do
    !print *, ''

    ! Check normalization
    !print *, 'Check:', sum( ModEvolStates(:,1) * h )

    ! Check normalization
    !do ii=1,Nt
    !    norm = sum( ModEvolStates(:,ii) * h )
    !    ModEvolStates(:,ii) = ModEvolStates(:,ii)/sqrt(norm)
    !    !print *, 'Final Check:', sum( ModEvolStates(:,ii) * h )
    !end do

    ! Write evolved states into a file
    !folder_name = 'results/'
    !file_name = 'evol_cmap.dat'
    !call write_cmap(folder_name,file_name,Time,x,ModEvolStates)

    ! deallocate all arrays
    deallocate(state) !, densMat)

end program demo
