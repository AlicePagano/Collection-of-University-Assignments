###############################################
# Example makefile for matrix
###############################################


# Make-Guide ----------------------------------

# Command			Result
# make				Compile the matrix_type and run the demo
# make matrix_type		Compile the module matrix_type
# make demo			Compile and run the demonstration program

# Choose element type: D (double real), or Z (double complex)
ELEM=D

CC = gfortran
CFLAGS = -cpp -DELEM=$(ELEM)
LIBS = #-I/usr/local/opt/lapack/include -L/usr/local/opt/lapack/lib -llapack #$(LDFLAGS)
DEBUG = -debug

OBJ  = src/error_handling.o src/error_handling_common_errors.o src/matrix_type.o src/debug_checkpoint.o 

all: error_handling error_handling_common_errors matrix_type debug_checkpoint demo

error_handling: src/error_handling.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

error_handling_common_errors: src/error_handling_common_errors.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

matrix_type: src/matrix_type.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

debug_checkpoint: src/debug_checkpoint.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

demo: $(OBJ)
		$(CC) $(OBJ) -o $@ demo.f90 $(LIBS)
		@echo 'Starting demonstration program...'
		@./demo $(DEBUG)
		@echo ' '

clean:
	-$(RM) $(OBJ) matrix_type.mod debug_checkpoint.mod error_handling.mod error_handling_common_errors.mod demo.o demo 
	-@echo ' '

.PHONY: all clean matrix_type debug_checkpoint demo
