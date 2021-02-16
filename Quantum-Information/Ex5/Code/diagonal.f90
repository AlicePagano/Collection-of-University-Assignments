! This program computes the eigenvalues of a random hermitian matrix of dimension N.
! Then, it computes the normalized spacing between eigenvalues and produce a histogram.

program demo

    use histogram
    IMPLICIT none


    ! matrix size
    integer :: n
    CHARACTER(len=:), allocatable :: Nchar
    integer :: arglen
    ! number of bins and histogram range
    integer :: nbins
    CHARACTER(len=:), allocatable :: Nbinschar
    real(8) :: minimum, maximum
    character(len=30) :: file_name
    ! number of times to compute random matrix
    integer :: ntime, tt
    CHARACTER(len=:), allocatable :: Ntimechar
    ! level for local average
    integer :: level
    CHARACTER(len=:), allocatable :: Levelchar
    ! matrix variable
    complex(8), dimension(:,:), allocatable :: A ! matrix
    real(8), dimension(:), allocatable :: AP  ! package matrix
    ! initialization variables
    integer, dimension(4) :: iseed
    real(4) :: u1, u2, u3, u4
    ! LAPACK zheev auxiliary variables
    integer :: info, lwork
    real(8), dimension(:), allocatable :: rwork
    complex(8), dimension(:), allocatable :: work
    ! eigenvalues variables
    real(8), dimension(:), allocatable :: eig, delta_eig, norm_delta_eig, local_norm_delta_eig, ri
    real(8) :: aver_delta_eig, aver_ri
    real(8), dimension(:), allocatable :: si, local_si
    integer :: ll, uu
    ! local average variables
    real(8), dimension(:), allocatable :: local_aver_delta_eig
    real(8) :: aver_sx, aver_dx
    ! iterators variable
    integer :: ii, jj, k
    ! check time variable
    real(4) :: t1, t2
    ! write data into file
    integer :: file=1 ! file variable
    logical :: exist
    character(len=10) :: folder_name

    ! Get matrix size as input from command line
    call GET_COMMAND_ARGUMENT(1, length=arglen)
    allocate(character(arglen) :: Nchar)
    call GET_COMMAND_ARGUMENT(1, value=Nchar)
    ! convert command line argument to integer (matrix size)
    read(Nchar(:),'(i5)') n

    ! Get number of bins as input from command line
    call GET_COMMAND_ARGUMENT(2, length=arglen)
    allocate(character(arglen) :: Nbinschar)
    call GET_COMMAND_ARGUMENT(2, value=Nbinschar)
    ! convert command line argument to integer (matrix size)
    read(Nbinschar(:),'(i5)') nbins

    ! Get number of bins as input from command line
    call GET_COMMAND_ARGUMENT(3, length=arglen)
    allocate(character(arglen) :: Ntimechar)
    call GET_COMMAND_ARGUMENT(3, value=Ntimechar)
    ! convert command line argument to integer (matrix size)
    read(Ntimechar(:),'(i5)') ntime

    ! Get number of bins as input from command line
    call GET_COMMAND_ARGUMENT(4, length=arglen)
    allocate(character(arglen) :: Levelchar)
    call GET_COMMAND_ARGUMENT(4, value=Levelchar)
    ! convert command line argument to integer (matrix size)
    read(Levelchar(:),'(i5)') level

    allocate(si(ntime*(n-1)))
    allocate(local_si(ntime*(n-1)))


    folder_name = trim('data/')
    ! check if the folder exist
    inquire(file=trim(folder_name), exist=exist)
    if (.not.exist) then
      call system('mkdir '//folder_name)
    end if

    file_name = trim('diagonal_data-'//Levelchar//'.dat')

    !print *, trim(folder_name//file_name)

    ! open the files for writing
    inquire(file=trim(folder_name//file_name), exist=exist)
    if (exist) then
      open(file, file=trim(folder_name//file_name), status="old", position="append", action="write")
    else
      open(file, file=trim(folder_name//file_name), status="new", action="write")
      write(file,*) '# time ri (level='//Levelchar//')'
    end if


    do tt=1,ntime

        ! allocate matrix A and vector AP
        allocate(A(n,n))
        allocate(AP(n))
        ! Random initialize diagonal matrix A

        A = 0

        ! 1) compute iseed randomly
        call random_number(u1)
        call random_number(u2)
        call random_number(u3)
        call random_number(u4)
        iseed = (/ FLOOR(4096*u1), FLOOR(4096*u2), FLOOR(4096*u3), 2*FLOOR(4096*u4)-1 /) ! n=0 and m=4095, the last must be odd
        ! 2) initialize random vector AP
        call dlarnv(3,iseed,n,AP)
        !print *, AP

        ! 3) unpacking the matrix AP to A
        ! (the lower triangle of A is stored column-by-column in AP)
        do ii=1,n
            A(ii,ii) = cmplx(AP(ii),0)
        end do
        ! Write matrix row-by-row
        !do ii=1,n
        !      print *, A(ii,:)
        !end do

        ! allocate and initialize LAPACK zheev variables
        lwork = max(1,2*n-1)
        allocate(work(max(1,lwork)))
        allocate(rwork(max(1,3*n-2)))
        work  = 0
        rwork = 0

        ! allocate and initialize eigenvalues variables
        allocate(eig(n))
        allocate(delta_eig(n-1))
        allocate(norm_delta_eig(n-1))
        allocate(local_norm_delta_eig(n-1))
        allocate(ri(n-1))
        eig            = 0
        delta_eig      = 0
        aver_delta_eig = 0
        norm_delta_eig = 0
        ri             = 0

        call cpu_time(t1)
        ! compute eigenvalues of matrix A (ordered in ASCENDING ORDER)
        call zheev('N', 'L', n, A, n, eig, WORK, LWORK, RWORK, info)
        call cpu_time(t2)
        !print *, t2-t1
        !print *, eig

        ! compute eigenvalues spacing
        do ii=1,n-1
            delta_eig(ii) = eig(ii+1) - eig(ii)
        end do
        !print *, delta_eig

        ! compute average spacing as a telescopic sum
        ! if eigenvalues are ordered in ascending order:
        aver_delta_eig = (eig(n) - eig(1)) / (n-1)
        !print *, aver_delta_eig

        ! compute eigenvalues NORMALIZED spacing
        do ii=1,n-1
            norm_delta_eig(ii) = delta_eig(ii)/aver_delta_eig
        end do
        !print *, norm_delta_eig

        ! add data of this esecution to spacing vector
        ll = (tt-1)*(n-1) + 1
        uu = tt*(n-1)
        si(ll:uu) = norm_delta_eig

        ! compute ri
        do ii=1,n-1
            ri(ii) = min(delta_eig(ii),delta_eig(ii+1)) / max(delta_eig(ii),delta_eig(ii+1))
        end do

        ! compute average r
        aver_ri = sum(ri) / size(ri)
        !print *, '<r> =', aver_ri

        ! write average of ri into a file
        write(file,*) tt, aver_ri

        ! compute local average spacing
        aver_sx = (eig(2*level+1+1)-eig(1)) / (2*level+1)
        aver_dx = (eig(n)-eig(n-2*level-1)) / (2*level+1)

        allocate(local_aver_delta_eig(n-1))
        local_aver_delta_eig = 0

        do ii=1,n-1
            if (ii <= level) then
                local_aver_delta_eig(ii) = aver_sx
                !local_aver_eig(ii) = sum(delta_eig(1:(2*level+1))) / (2*level+1)
            else if (ii > n-1-level) then
                local_aver_delta_eig(ii) = aver_dx
            else
                local_aver_delta_eig(ii) = (eig(ii+level+1)-eig(ii-level)) / (2*level+1)
            endif
        end do
        !print *, local_aver_delta_eig

        ! compute eigenvalues local NORMALIZED spacing
        do ii=1,n-1
            local_norm_delta_eig(ii) = delta_eig(ii)/local_aver_delta_eig(ii)
        end do

        ! add data of this esecution to spacing vector
        ll = (tt-1)*(n-1) + 1
        uu = tt*(n-1)
        local_si(ll:uu) = local_norm_delta_eig

        ! deallocate all arrays
        deallocate(A, AP)
        deallocate(work,rwork)
        deallocate(eig,delta_eig)
        deallocate(local_aver_delta_eig)
        deallocate(norm_delta_eig)
        deallocate(local_norm_delta_eig)
        deallocate(ri)

        print *, repeat("+",tt)

    end do


    folder_name = trim('hist_data/')

    ! create histogram of normalized spacing between eigenvalues
    minimum = 0 !minval(si)
    maximum = 5 !maxval(si)
    !print *, minimum
    !print *, maximum
    file_name = trim('diagonal-global-'//Levelchar//'.dat')
    call create_histogram(si,nbins,minimum,maximum,file_name,folder_name)

    ! create histogram of local normalized spacing between eigenvalues
    minimum = 0 !minval(local_si)
    maximum = 5 !maxval(local_si)
    !print *, minimum
    !print *, maximum
    file_name = trim('diagonal-local-'//Levelchar//'.dat')
    call create_histogram(local_si,nbins,minimum,maximum,file_name,folder_name)

    deallocate(si)
    deallocate(local_si)
    close(file)

end program demo
