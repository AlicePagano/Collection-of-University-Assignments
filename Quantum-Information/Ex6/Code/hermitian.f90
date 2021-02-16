! This program computes the eigenvalues and eigenfuncitons of a matrix of an harmonic oscillator.
! To compile: gfortran -o hermitian_pro hermitian_pro.f90 -llapack
! To run (example): ./hermitian_pro 3 -5.00 5.00
program demo

    IMPLICIT none

    ! discretization variables
    integer :: N
    CHARACTER(len=:), allocatable :: Nchar
    integer :: arglen
    real(8) :: min, max
    CHARACTER(len=:), allocatable :: Minchar, Maxchar
    real(8) :: h
    ! matrix variable
    real(8), dimension(:,:), allocatable :: A ! matrix
    real(8), dimension(:), allocatable :: Ad ! diagonal part of matrix
    real(8), dimension(:), allocatable :: Asubd ! subdiagonal part of matrix
    ! LAPACK zheev auxiliary variables
    integer :: info, lwork
    complex(8), dimension(:), allocatable :: work
    ! eigenvalues variables
    real(8), dimension(:), allocatable :: eig
    ! iterators variable
    integer :: ii, jj, k
    ! check time variable
    real(4) :: t1, t2
    ! write data into file
    integer :: file=1, file2=2 ! file variable
    logical :: exist
    character(len=12) :: file_name
    character(len=8) :: folder_name


    ! Get matrix size as input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: Nchar)
    call GET_COMMAND_ARGUMENT(1, value=Nchar)
    ! convert command line argument to integer (matrix size)
    read(Nchar(:),'(i5)') n

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



    folder_name = trim('results/')
    ! check if the folder exist
    inquire(file=trim(folder_name), exist=exist)
    if (.not.exist) then
      call system('mkdir '//folder_name)
    end if

    file_name = trim('eig_func.dat')

    !print *, trim(folder_name//file_name)

    ! open file for writing
    inquire(file=trim(folder_name//file_name), exist=exist)
    if (exist) then
      open(file, file=trim(folder_name//file_name), status="old", position="rewind", action="write")
    else
      open(file, file=trim(folder_name//file_name), status="new", action="write")
    end if

    file_name = trim('eig_val.dat')

    !print *, trim(folder_name//file_name)

    ! open file for writing
    inquire(file=trim(folder_name//file_name), exist=exist)
    if (exist) then
      open(file2, file=trim(folder_name//file_name), status="old", position="rewind", action="write")
    else
      open(file2, file=trim(folder_name//file_name), status="new", action="write")
    end if


    ! print range
    print *, 'Min:', min
    print *, 'Max:', max

    ! compute discretization step
    h = (max-min)/N
    ! print discretization step
    print *, 'h:', h
    print *, ''

    ! allocate matrix A
    allocate(Ad(0:N))
    allocate(Asubd(0:N-1))

    ! initialize hermitian matrix A
    do ii=0,N
        Ad(ii) = 1/(h**2) + 0.5*(min + ii*h)**2
        if(ii<N) then
            Asubd(ii) = -1/(2*h**2)
        end if
    end do

    ! Write matrix row-by-row
    !print *, 'Initial matrix:'
    !do ii=0,n
    !      print *, A(ii,:)
    !end do
    !print *, ''

    ! allocate and initialize LAPACK zheev variables
    lwork = 2*(N+1)-2 !max(1,2*N-1)

    allocate(work(lwork))
    work  = 0

    allocate(A(0:N,0:N))

    call cpu_time(t1)
    ! compute eigenvalues and eigefunctions
    call dstev('V', N+1, Ad, Asubd, A, N+1, WORK, info)
    call cpu_time(t2)
    print *, 'Elapsed time:', t2-t1
    print *, ''
    !print *, 'Info:', info

    ! Print eigenvalues
    print *, 'Eigenvalues:'
    !print *, eig

    do ii=0,5
        print *, Ad(ii)
        print *, (float(ii) + 0.5)
        print *, ''
    enddo

    print *, ''
    ! Write matrix row-by-row
    !print *, 'Final matrix:'
    !do ii=1,n
    !      print *, real(A(ii,:))
    !end do

    ! Normalize eigenfunction matrix
    A = A * sqrt( (N+1)/ (2*max) )


    ! Write eigenfunctions into file
    do ii=0,n
        write(file,*) (min+ii*h), real(A(ii,:))
    end do

    ! Write eigenvalues into file
    do ii=0,n
        write(file2,*) ii, Ad(ii), (float(ii) + 0.5)
    end do

    ! deallocate all arrays
    deallocate(Ad,Asubd)
    deallocate(work)
    ! close file
    close(file)

end program demo
