EXERCISE 8
-------------------------------------------------------------------------------------------------------------------

Student: Alice Pagano
Number : 1236916

-------------------------------------------------------------------------------------------------------------------

The files are organized as follow:

+ /Code :

	- density_matrix.f90 : code for initializing state, compute density matrix and reduced density matrix

		To compile and run:

		- gfortran -o density_matrix density_matrix.f90

		- ./density_matrix 2 2 F output.dat

	- density_matrix_init.f90 : code for initializing generic and separable state and compute elapsed time

	- script.py : python script for compiling and running the code "density_matrix_init.f90" for different N and D

	- time.p :  gnuplot script for plotting elapsed time results

+ /Plots :

	- /result_non-sep : data with time required for initializing a generic state

	- /result_sep : data with time required for initializing a separable state

	- time.p : script for doing plots

	- /results : contain example of state, density matrix and reduced density matrix
	
	- *.pdf : plots

+ Ex8_Alice-Pagano.pdf: report of the exercise.

+ Ex8.pdf : exercise.
