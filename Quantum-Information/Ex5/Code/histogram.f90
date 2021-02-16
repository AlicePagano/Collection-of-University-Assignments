! This module creates a histogram

module histogram

    contains

        !> <b> Subroutine to create an histogram </b>
        !
        ! ========== DOCUMENTATION ==========
        !
        ! Definition:
        ! ===========
        !
        ! Ssubroutine create_histogram(events,n_bins,min,max)
        !
        ! This subroutine create an histogram.
        !
        ! INPUT VARIABLES:
        !   real(8), dimension(:): events  entries of the histogram
        !   integer :: n_bins  number of bins for the histogram
        !   real(8) :: min, max  minimum and maximum range
        !   character(len=*) :: file name
        !   character(len=*) :: folder name
        subroutine create_histogram(events,n_bins,min,max,file_name,folder_name)

            implicit none
            ! input variables
            real(8), dimension(:), intent(in) :: events
            integer, intent(in) :: n_bins
            real(8), intent(in) :: min, max
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in) :: folder_name
            ! other variables
            integer :: ii, jj ! iterators
            integer :: file=1 ! file variable
            logical :: exist
            !character(len=10) :: folder_name
            real(8) :: dx, tot_area
            !real(8), dimension(n_bins)   :: bin_area
            real(8), dimension(n_bins) :: bin_centers
            real(8), dimension(n_bins+1) :: bin_edges
            real(8), dimension(n_bins) :: hist, norm_hist

            ! initialize variables to zero
            !bin_area    = 0
            bin_centers = 0
            bin_edges   = 0
            hist        = 0
            norm_hist   = 0

            ! compute bin width
            dx = (max-min) / n_bins

            ! compute bin centers
            do ii=1,n_bins
                bin_centers(ii) = min + dx/2 + (ii-1)*dx
            end do

            ! compute left bin edges (plus the most right edge)
            do ii=1,n_bins+1
                bin_edges(ii) = min + (ii-1)*dx
            end do

            ! compute histogram
            do ii=1,n_bins
                ! fill bins with events
                do jj=1,size(events,1)
                    if (events(jj)>=bin_edges(ii) .and. events(jj)<=bin_edges(ii+1)) then
                        hist(ii) = hist(ii) + 1
                    end if
                end do
                ! compute bin area
                !bin_area(ii) = hist(ii) * dx
            end do

            ! compute total histogram area
            tot_area = size(events) * dx !sum(bin_area)

            ! compute normalized histogram
            do ii=1,n_bins
                norm_hist(ii) = hist(ii)/tot_area
            end do

            ! check if the histogram is normalized
            !print *, sum(norm_hist)*dx

            ! check if the folder exist
            inquire(file=trim(folder_name), exist=exist)
            if (.not.exist) then
              call system('mkdir '//trim(folder_name))
            end if

            ! open the files for writing
            inquire(file=trim(folder_name//file_name), exist=exist)
            if (exist) then
              open(file, file=trim(folder_name//file_name), status="old", position="append", action="write")
              write(file,*) '#'
            else
              open(file, file=trim(folder_name//file_name), status="new", action="write")
              write(file,*) '# left_edges centers entries norm_entries'
            end if

            ! open file
            !open(file, file=trim(folder_name//trim(file_name)), action="write")
            !write(file,*) '# left_edges centers entries norm_entries'

            ! write data into file
            do ii=1,n_bins
                write(file,*) bin_edges(ii), bin_centers(ii), hist(ii), norm_hist(ii)
            end do

            close(file)
            return

        end subroutine create_histogram

end module histogram
