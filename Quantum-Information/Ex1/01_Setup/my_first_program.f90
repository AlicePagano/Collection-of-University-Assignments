! To compile: gfortran my_first_program.f90 -o prova.x
! To run: ./prova.x
program my_first_program

    implicit none
    integer, parameter :: file = 11

    open(unit=file,file="my_file.txt",form='formatted')
    write (file, *) "Hello world!"
    close (file)

end program my_first_program
