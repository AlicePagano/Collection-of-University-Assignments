###############################################
# Example makefile for matrix
###############################################


# Make-Guide ----------------------------------

# Command			Result
# make				Compile the matrix_type and run the demo
# make matrix_type		Compile the module matrix_type
# make demo			Compile and run the demonstration program

# Choose element type: D (double real), or Z (double complex)
ELEM=Z

CC = gfortran
CFLAGS = -cpp -DELEM=$(ELEM)

DEPS = src/matrix_type.f90
OBJ = src/matrix_type.o

all: matrix_type demo

matrix_type: $(DEPS)
		$(CC) -c -o src/$@.o $< $(CFLAGS)
		@echo ' '

demo: $(OBJ)
		$(CC) $(OBJ) -o $@ demo.f90
		@echo 'Starting demonstration program...'
		@./demo
		@echo ' '

clean:
	-$(RM) $(OBJ) matrix_type.mod demo.o demo matrix_*.txt
	-@echo ' '

.PHONY: all clean matrix_type demo
