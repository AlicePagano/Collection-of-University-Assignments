EXERCISE 7
-------------------------------------------------------------------------------------------------------------------

Student: Alice Pagano
Number : 1236916

-------------------------------------------------------------------------------------------------------------------

The files are organized as follow:

+ /Code :

	- state.p : gnuplot script for plotting real and imaginary part of wavefunction.

	- state_gif.p :  gnuplot script for plotting real and imaginary part of wavefunction as a gif.

	- cmap.p : gnuplot script for plotting a colormap of square modulus of the wave function

	- split_operator.90 : main program.

		To compile and run:

		- gfortran -o split_operator split_operator.f90 -L/usr/local/opt/lapack/lib -llapack -L/usr/local/lib -lfftw3

		- ./split_operator 1000 -5.00 5.00 1000 8.00

+ /Results :

	- /plot : resulting plots

	- /data : simulation data for 1000 -5.00 5.00 1000 8.00

+ Ex7_Alice-Pagano.pdf: report of the exercise.

+ Ex7.pdf : exercise.
