! Main program
!
program demo

    USE MATRIX_TYPE
    USE DEBUG_CHECKPOINT
    USE ERROR_HANDLING_COMMON_ERRORS
    IMPLICIT none

    ! Define variables
    logical :: DEBUG, exist
    TYPE(Matrix) :: A, B, C
    INTEGER, dimension(2) :: N
    INTEGER, dimension(1) :: N1
    CHARACTER(len=:), allocatable :: Nchar
    integer(4) :: arglen
    character(len=:), allocatable :: debug_option
    real(8) :: t1,t2,dt
    integer :: file=1
    CHARACTER(len=:), allocatable :: file_name ! File name
    character(len=7) :: folder_name='./time/'
    integer :: N_times = 6 ! Sampling times
    integer :: tt
    real(8), dimension(:), allocatable :: time
    integer :: p1, p2 ! iterators
    real :: average, variance, std_deviation, sum1, sum2


    ! Get input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: Nchar)
    call GET_COMMAND_ARGUMENT(1, value=Nchar)
    ! convert command line argument to integer (matrix size)
    read(Nchar(:),'(i5)') N1

    ! Get file name as input from command line
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: file_name)
    call GET_COMMAND_ARGUMENT(2, value=file_name)

    ! Enable -debug flag in the compiler
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: debug_option)
    call GET_COMMAND_ARGUMENT(2, value=debug_option)
    if (debug_option == "-debug") then
            DEBUG = .TRUE.
        else
            DEBUG = .FALSE.
    end if

    N=(/N1,N1/) ! Matrix Size

    ! Initialize matrix A, B, C
    CALL MatInit(A, N, ctrl='R') ! Random initialization
    CALL MatInit(B, N, ctrl='R') ! Random initialization
    CALL MatInit(C, N, ctrl='O') ! Initialize with zeros

    ! Check matrix A and B
    call check_matrix(debug,A,file_name=__FILE__,line=__LINE__)
    call check_matrix(debug,B,file_name=__FILE__,line=__LINE__)

    ! allocate time list
    allocate(time(N_TIMES))

    ! check if the folder exist
    inquire(file=folder_name, exist=exist)
    if (.not.exist) then
      call system('mkdir '//folder_name)
    end if

    ! open the files for writing
    inquire(file=folder_name//file_name, exist=exist)
    if (exist) then
      open(file, file=folder_name//file_name, status="old", position="append", action="write")
    else
      open(file, file=folder_name//file_name, status="new", action="write")
      write(file,*) '# ','N ', 'samples ', 'average ', 'error '
    end if

    do tt=1,N_times
        ! Matrix-matrix multiplication with user defined function
        call cpu_time(t1)
        CALL MatMultbyCol(A,B,C) ! Matrix-matrix multiplication by column
        call cpu_time(t2)

        dt = t2-t1 ! time of matrix-matrix multiplication by column
        time(tt) = dt
    end do

    average       = 0
    variance      = 0
    std_deviation = 0
    sum1          = 0
    sum2          = 0

    if (N_TIMES>1) then

        do p1=2,N_TIMES
            sum1 = sum1 + time(p1)
        end do

        average = sum1 / real(N_TIMES-1);

        do p2=2,N_TIMES
            sum2 = sum2 + (time(p2) - average)**2
        end do

        variance = sum2 / real(N_TIMES-1)
        std_deviation = sqrt(variance)

        write(file,*) N1, N_TIMES-1, average, std_deviation

    end if

    deallocate(time)
    close(file)

    ! Check matrix C
    call check_matrix(debug,C,file_name=__FILE__,line=__LINE__)

    ! Delete matrix A, B and C
    CALL MatDel(A)
    CALL MatDel(B)
    CALL MatDel(C)

end program demo
