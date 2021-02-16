###############################################
# Example makefile for matrix
###############################################


# Make-Guide ----------------------------------

# Command			Result
# make				Compile the matrix_type and run the demo
# make clean			Delete files 


# Choose element type: D (double real), or Z (double complex)
ELEM=D
# Debug options
DEBUG = -debug

# Compiler
CC = gfortran
CFLAGS = -cpp -DELEM=$(ELEM)
LIBS = #-I/usr/local/opt/lapack/include -L/usr/local/opt/lapack/lib -llapack #$(LDFLAGS)
FAST = 

OBJ  = src/error_handling.o src/error_handling_common_errors.o src/matrix_type.o src/debug_checkpoint.o 


EXAMPLE_PATH = matrix_multiplication/
EXAMPLE = mat_mult_row mat_mult_col mat_mult_mat
FILE = 

all: error_handling error_handling_common_errors matrix_type debug_checkpoint $(EXAMPLE)

error_handling: src/error_handling.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

error_handling_common_errors: src/error_handling_common_errors.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

matrix_type: src/matrix_type.f90
		$(CC) $(FAST) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

debug_checkpoint: src/debug_checkpoint.f90
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

$(EXAMPLE): $(OBJ)	
		$(CC) $(FAST) $(OBJ) -o $@ $(EXAMPLE_PATH)$@.f90 $(LIBS) -cpp

clean:
	-$(RM) $(OBJ) matrix_type.mod debug_checkpoint.mod error_handling.mod error_handling_common_errors.mod demo.o demo $(EXAMPLE) time/*.dat
	rmdir time
	-@echo ' '

.PHONY: all clean
