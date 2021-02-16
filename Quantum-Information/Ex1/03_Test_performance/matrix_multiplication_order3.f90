! To compile: gfortran matrix_multiplication.f90

module time_keeper
  implicit none
  integer    :: start(8), now(8)

contains

  subroutine startclock()
    implicit none
    call date_and_time(values=start)
  end subroutine startclock


  subroutine elapsedtime_s( et_s )
    implicit none
    integer :: diffs(8)=0
    real(8), intent(out):: et_s       ! in seconds

    call date_and_time(values=now)

    ! - Find the difference in times
    diffs = now - start

    ! - This works only when the time is measured in a specific month
    if (diffs(3) > 0) then
       diffs(5) = 24*diffs(3) + diffs(5)
    endif

    et_s = diffs(5) * 3600 + diffs(6) * 60 + diffs(7) + 1e-3 * diffs(8)

  end subroutine elapsedtime_s

end module time_keeper




program matrix_multiplication

  use time_keeper
  implicit none
  integer :: i, j, k, it, ms
  integer, parameter :: file = 11
  integer :: m,n,p
  real(4), dimension (:,:), allocatable :: A, B, C
  real(4), dimension(:), allocatable :: matrix_dimension
  real(8) :: dt_cpu
  ! Variables for fixing the seed
  integer :: seed_size
  integer,allocatable :: seed(:)

  ! open the file for writing
  open(file,file='time_cpu_3.txt',form='formatted')

  write(file,*) 'm n p time(s)'

  ! Inizialize vector with matrix dimensions
  ms = 10
  allocate(matrix_dimension(ms))
  matrix_dimension = (/ 100,200,300,400,500,600,700,800,900,1000 /)

  do it=1,ms

      m = matrix_dimension(it)
      n = matrix_dimension(it)
      p = matrix_dimension(it)

      write(file,fmt="(i6,i6,i6)",ADVANCE="NO") m, n, p

      allocate(A(m,n),B(n,p),C(m,p))

      ! Set seed
      call random_seed() ! initialize with system generated seed
      call random_seed(size=seed_size) ! find out size of seed
      allocate(seed(seed_size))
      call random_seed(get=seed) ! get system generated seed
      !write(*,*) seed            ! writes system generated seed
      seed=314159265
      call random_seed(put=seed) ! set current seed
      call random_seed(get=seed) ! get current seed
      !write(*,*) seed            ! writes 314159265
      deallocate(seed)

      ! Inizialize random matrix A and B and set all elements of C=0
      call RANDOM_NUMBER(A)
      call RANDOM_NUMBER(B)
      C = 0

      ! Start time count
      call startclock()


      do k=1,n
          do i=1,m
              do j=1,p
                  C(i,j) = A(i,k) * B(k,j) + C(i,j)
              end do
          end do
      end do

      ! End time count
      call elapsedtime_s(dt_cpu)
      write(file,*) dt_cpu

      deallocate (A, B, C)
  end do

  deallocate(matrix_dimension)
  close(file)
end program matrix_multiplication
