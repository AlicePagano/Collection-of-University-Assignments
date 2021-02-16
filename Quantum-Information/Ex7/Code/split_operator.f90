! This program computes the evolved ground state for time-dependent harmonic oscillator.

! To compile: gfortran -o split_operator split_operator.f90 -L/usr/local/opt/lapack/lib -llapack -L/usr/local/lib -lfftw3

! To run (example): ./split_operator 1000 -5.00 5.00 1000 8.00

module split_operator

    contains

        ! Initialize harmonic oscillator matrix
        subroutine harm_matrix(min, max, pos, Mat)

            implicit none
            real(8), intent(in) :: min, max
            real(8) :: h
            complex(8), dimension(:,:), intent(out) :: Mat
            real(8), dimension(:), allocatable, intent(out) :: pos
            integer :: N, Nslice
            integer :: ii

            N = size(Mat,1)
            Nslice = N-1

            allocate(pos(N))

            ! compute discretization step
            h = (max-min)/Nslice

            print *, 'Nslice:', Nslice
            print *, 'Min:', min
            print *, 'Max:', max
            print *, 'h:', h
            print *, ''

            ! initialize vector of positions and matrix to all zero
            pos = 0
            Mat = 0

            ! initialize harmonic oscillator matrix and position vector
            do ii=1,N
                Mat(ii,ii) = 1/(h**2) + 0.5*(min + (ii-1)*h)**2
                pos(ii)   = min + (ii-1)*h
                if(ii<N) then
                    Mat(ii+1,ii) = -1/(2*h**2)
                    Mat(ii,ii+1) = -1/(2*h**2)
                end if
            end do

        end subroutine harm_matrix


        ! Find eigenfunctions and eigenvalues
        subroutine find_eigenstate(min,max,Mat,eig)

            implicit none
            complex(8), dimension(:,:), intent(inout) :: Mat
            real(8), dimension(:), allocatable, intent(out) :: eig
            real(8), intent(in) :: min, max
            real(8) :: h
            integer :: N, Nslice
            integer :: ii
            real(8) :: norm
            ! LAPACK zheev auxiliary variables
            integer :: info, lwork
            real(8), dimension(:), allocatable :: rwork
            complex(8), dimension(:), allocatable :: work
            ! check time variable
            real(4) :: t1, t2

            N = size(Mat,1)
            Nslice = N-1

            ! compute discretization step
            h = (max-min)/Nslice

            ! allocate and initialize LAPACK zheev variables
            lwork = 2*N-1
            allocate(work(lwork))
            allocate(rwork(3*N-2))
            work  = 0
            rwork = 0

            ! allocate and initialize eigenvalues variables
            allocate(eig(N))
            eig = 0

            ! compute eigenfunctions and eigenvaluesvalues of matrix A (ordered in ASCENDING ORDER)
            call cpu_time(t1)
            call zheev('V', 'L', N, Mat, N, eig, WORK, LWORK, RWORK, info)
            call cpu_time(t2)
            print *, 'Elapsed time for diagonalization:', t2-t1
            print *, ''

            ! Normalization
            do ii=1,N
                norm = real( sum( (Mat(:,ii)*conjg(Mat(:,ii))  * h) ) )
                Mat(:,ii) = Mat(:,ii)/sqrt(norm)
                !print *, 'Check:', sum( (Mat(:,ii)*conjg(Mat(:,ii))  * h) )
            end do


            deallocate(work,rwork)

        end subroutine find_eigenstate


        ! Potential energy (coordinate space)
        subroutine potential(N, min, max, t, tmax, VecPot)

            implicit none
            integer, intent(in) :: N
            real(8), intent(in) :: min, max
            real(8) :: h
            real(8), intent(in) :: t, tmax
            real(8), dimension(:), allocatable, intent(inout) :: VecPot
            integer :: Nslice ! number of slices
            integer :: ii

            Nslice = N-1
            ! compute discretization step
            h = (max-min)/Nslice

            allocate(VecPot(N))
            VecPot = 0

            do ii=1,N
           	    VecPot(ii) =  0.5*( (min + (ii-1)*h)-(t/tmax) )**2
            end do

        end subroutine potential


        ! Kinetic energy (momentum space)
        subroutine kinetic(N, min, max, VecKin)

            implicit none
            integer, intent(in) :: N
            real(8), intent(in) :: min, max
            real(8) :: pmin, pmax
            real(8) :: h
            real(8), dimension(:), allocatable, intent(out) :: VecKin
            integer :: Nslice ! number of slices
            integer :: ii
            real(8) :: pi = 4.D0*DATAN(1.D0)

            Nslice = N-1
            ! compute space discretization step
            h = (max-min)/Nslice

            allocate(VecKin(N))
            VecKin = 0

            do ii=1,Nslice+1
                if(ii<=int(Nslice/2)) then
           	        VecKin(ii) =  0.5 * ( 2d0*pi*ii/(max-min) )**2
                else if (ii>int(Nslice/2)) then
                    VecKin(ii) =  0.5 * ( 2d0*pi*(ii-Nslice-1)/(max-min) )**2
                end if
            end do

        end subroutine kinetic


        subroutine split_op_method(min,max,t,dt,tmax,WaveInit,WaveFin)

            implicit none
            real(8), intent(in) :: min, max
            real(8), intent(in) :: t, dt, tmax
            complex(8), dimension(:), intent(in) :: WaveInit
            complex(8), dimension(:), intent(inout) :: WaveFin
            complex(8), dimension(:), allocatable :: Wave_FFT
            real(8), dimension(:), allocatable :: VecPot, VecKin
            integer :: N, Nslice
            real(8) :: h
            real(8) :: norm
            integer :: ii
            ! Fourier Transform variables
            integer(8) :: plan

            N = size(WaveInit,1)
            Nslice = N-1  ! number of slices

            ! compute space discretization step
            h = (max-min)/Nslice

            allocate(Wave_FFT(N))
            Wave_FFT = 0

            WaveFin = 0

            call potential(N, min, max, t, tmax, VecPot)
            call kinetic(N, min, max, VecKin)

            ! Multiply wave function by potential operator
            do ii=1,N
                WaveFin(ii) = cexp(cmplx(0.0,-0.5*VecPot(ii)*dt)) * WaveInit(ii)
            end do

            ! Normalization
            norm = sum( (WaveFin(:)*conjg(WaveFin(:)))*h )
            WaveFin(:) = WaveFin(:)/sqrt(norm)
            !print *, 'Check 1:', sum( (WaveFin(:)*conjg(WaveFin(:)))*h )

            ! Fourier transform (from coordinate space to momentum space)
            call dfftw_plan_dft_1d(plan, N, WaveFin, Wave_FFT, -1, 64)
      		call dfftw_execute_dft(plan, WaveFin, Wave_FFT)
     	    call dfftw_destroy_plan(plan)

            WaveFin = Wave_FFT

            ! Normalization
            norm = sum( (WaveFin(:)*conjg(WaveFin(:)))*h )
            WaveFin(:) = WaveFin(:)/sqrt(norm)
            !print *, 'Check 2:', sum( (WaveFin(:)*conjg(WaveFin(:)))*h )


            ! Multiply wave function by kinetic operator
            do ii=1,N
                WaveFin(ii) = cexp(cmplx(0.0,-1.0*VecKin(ii)*dt)) * WaveFin(ii)
            end do

            ! Normalization
            norm = sum( (WaveFin(:)*conjg(WaveFin(:)))*h )
            WaveFin(:) = WaveFin(:)/sqrt(norm)
            !print *, 'Check 3:', sum( (WaveFin(:)*conjg(WaveFin(:)))*h )

            Wave_FFT = 0

            ! Fourier anti-transform (from momentum space to coordinate space)
            call dfftw_plan_dft_1d(plan, N, WaveFin, Wave_FFT, 1, 64)
            call dfftw_execute_dft(plan, WaveFin, Wave_FFT)
            call dfftw_destroy_plan(plan)

            WaveFin = Wave_FFT

            ! Normalization
            norm = sum( (WaveFin(:)*conjg(WaveFin(:)))*h )
            WaveFin(:) = WaveFin(:)/sqrt(norm)
            !print *, 'Check 4:', sum( (WaveFin(:)*conjg(WaveFin(:)))*h )

            ! Multiply wave function by potential operator
            do ii=1,N
                WaveFin(ii) = cexp(cmplx(0.0,-0.5*VecPot(ii)*dt)) * WaveFin(ii)
            end do

            ! Normalization
            norm = sum( (WaveFin(:)*conjg(WaveFin(:)))*h )
            WaveFin(:) = WaveFin(:)/sqrt(norm)
            !print *, 'Check 5:', sum( (WaveFin(:)*conjg(WaveFin(:)))*h )

            deallocate(Wave_FFT)

        end subroutine split_op_method

end module split_operator






program demo

    use split_operator
    use file
    IMPLICIT none

    ! discretization variables
    integer :: N, Nslice
    CHARACTER(len=:), allocatable :: Nchar
    integer :: arglen
    real(8) :: min, max
    real(8) :: h
    CHARACTER(len=:), allocatable :: Minchar, Maxchar, Ntchar, tsimchar
    ! matrix variable
    complex(8), dimension(:,:), allocatable :: A ! matrix
    real(8), dimension(:), allocatable :: x ! pos
    complex(8), dimension(:,:), allocatable :: EvolStates
    real(8), dimension(:,:), allocatable :: ModEvolStates
    real(8) :: norm
    ! eigenvalues variables
    real(8), dimension(:), allocatable :: eig
    ! time discretization variables
    real(8), dimension(:), allocatable :: Time
    real(8) :: dt, tsim, tmax
    integer :: Nt, Ntslice
    ! iterators variable
    integer :: ii, jj, k
    ! file variables
    character(len=8) :: folder_name
    character(len=14) :: file_name


    ! Get matrix size as input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: Nchar)
    call GET_COMMAND_ARGUMENT(1, value=Nchar)
    ! convert command line argument to integer (matrix size)
    read(Nchar(:),'(i5)') Nslice

    ! Get min as input from command line
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: Minchar)
    call GET_COMMAND_ARGUMENT(2, value=Minchar)
    ! convert command line argument to integer (matrix size)
    read(Minchar(:),'(f5.2)') min

    ! Get max as input from command line
    call GET_COMMAND_ARGUMENT(3, length=arglen)
    allocate(character(arglen) :: Maxchar)
    call GET_COMMAND_ARGUMENT(3, value=Maxchar)
    ! convert command line argument to integer (matrix size)
    read(Maxchar(:),'(f5.2)') max

    ! Get max as input from command line
    call GET_COMMAND_ARGUMENT(4, length=arglen)
    allocate(character(arglen) :: Ntchar)
    call GET_COMMAND_ARGUMENT(4, value=Ntchar)
    ! convert command line argument to integer (matrix size)
    read(Ntchar(:),'(i5)') Ntslice

    ! Get max as input from command line
    call GET_COMMAND_ARGUMENT(5, length=arglen)
    allocate(character(arglen) :: tsimchar)
    call GET_COMMAND_ARGUMENT(5, value=tsimchar)
    ! convert command line argument to integer (matrix size)
    read(tsimchar(:),'(f5.2)') tsim


    ! Write matrix row-by-row
    !print *, 'Initial matrix:'
    !do ii=1,N
    !      print *, A(ii,:)
    !end do
    !print *, ''

    N  = Nslice + 1
    Nt = Ntslice + 1

    ! compute space discretization step
    h = (max-min)/Nslice

    allocate(A(N,N))
    allocate(EvolStates(N,Nt))
    allocate(ModEvolStates(N,Nt))
    A = 0
    EvolStates = 0
    ModEvolStates = 0

    ! Initialize harmonic oscillator matrix
    call harm_matrix(min, max, x, A)

    ! Find eingenstates
    call find_eigenstate(min,max,A,eig)

    ! Fix maximum time of the simulation
    tmax = 2.0

    ! Discretize time
    allocate(Time(Nt))
    Time = 0
    dt = tsim/Ntslice
    do ii=1,Nt
        Time(ii) = dt * (ii-1)
    end do

    print *, 'Ntslice:', Ntslice
    print *, 'tmax:', tsim
    print *, 'tmax:', tmax
    print *, 'dt:', dt
    print *, ''

    ! Initialize ground state
    EvolStates(:,1) = A(:,1)
    ModEvolStates(:,1) = A(:,1) * conjg(A(:,1))

    ! Check normalization
    !print *, 'Check:', sum( ModEvolStates(:,1) * h )

    do ii=2,Nt
        call split_op_method(min,max,Time(ii),dt,tmax,EvolStates(:,ii-1),EvolStates(:,ii))
        ModEvolStates(:,ii) = real(EvolStates(:,ii)*conjg(EvolStates(:,ii)))
    end do

    ! Check normalization
    !do ii=1,Nt
    !    norm = sum( ModEvolStates(:,ii) * h )
    !    ModEvolStates(:,ii) = ModEvolStates(:,ii)/sqrt(norm)
    !    !print *, 'Final Check:', sum( ModEvolStates(:,ii) * h )
    !end do

    ! Write times into a file
    folder_name = 'results/'
    file_name = 'evol_time.dat'
    call write_time(folder_name,file_name,Time)

    ! Write evolved states into a file (real part)
    folder_name = 'results/'
    file_name = 'evol_real.dat'
    call write_states(folder_name,file_name,x,real(EvolStates,8))

    ! Write evolved states into a file (imaginary part)
    folder_name = 'results/'
    file_name = 'evol_imag.dat'
    call write_states(folder_name,file_name,x,aimag(EvolStates))

    ! Write evolved states into a file
    folder_name = 'results/'
    file_name = 'evol_cmap.dat'
    call write_cmap(folder_name,file_name,Time,x,ModEvolStates)

    ! deallocate all arrays
    deallocate(A,x)
    deallocate(eig)
    deallocate(Time,EvolStates)

end program demo




module file

    contains

        subroutine write_cmap(folder_name,file_name,time,x,Mat)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            real(8), dimension(:), intent(in) :: time, x
            real(8), dimension(:,:), intent(in) :: Mat
            integer :: ii, jj

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
              open(file, file=trim(folder_name//file_name), status="old", position="rewind", action="write")
            else
              open(file, file=trim(folder_name//file_name), status="new", action="write")
            end if

            ! Write matrix row-by-row (with position column)
            do jj=1,size(time)
                do ii=1,size(Mat,1)
                    write(file,*) time(jj), x(ii), Mat(ii,jj)
                end do
            end do

            close(file)

        end subroutine write_cmap


        subroutine write_states(folder_name,file_name,Vec,Mat)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            real(8), dimension(:), intent(in) :: Vec
            real(8), dimension(:,:), intent(in) :: Mat
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
              open(file, file=trim(folder_name//file_name), status="old", position="rewind", action="write")
            else
              open(file, file=trim(folder_name//file_name), status="new", action="write")
            end if

            ! Write matrix row-by-row (with position column)
            do ii=1,size(Mat,1)
                  write(file,*) Vec(ii), Mat(ii,:)
            end do

            close(file)

        end subroutine write_states


        subroutine write_time(folder_name,file_name,Vec)

            implicit none
            integer :: file=1 ! file variable
            logical :: exist
            character(len=*), intent(inout) :: file_name
            character(len=*), intent(inout) :: folder_name
            real(8), dimension(:), intent(in) :: Vec
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
              open(file, file=trim(folder_name//file_name), status="old", position="rewind", action="write")
            else
              open(file, file=trim(folder_name//file_name), status="new", action="write")
            end if

            ! Write matrix row-by-row (with position column)
            do ii=1,size(Vec)
                  write(file,*) Vec(ii)
            end do

            close(file)

        end subroutine write_time


end module file
