###############################################
# Example makefile for matrix
###############################################


# Make-Guide ----------------------------------

# Command			Result
# make				Compile
# make clean			Delete files 


# Compiler
CC = gfortran
LIBS = -llapack
FAST = 

OBJ  = histogram.o
PROGRAM = hermitian diagonal


all: histogram $(PROGRAM)

histogram: histogram.f90
		$(CC) -c -o $@.o $<
		@echo ' '

$(PROGRAM): $(OBJ)	
		$(CC) $(FAST) $(OBJ) -o $@ $@.f90 $(LIBS)

clean:
	-$(RM) $(OBJ) histogram.mod $(PROGRAM) hist_data/*.dat data/*.dat
	rmdir hist_data
	rmdir data
	-@echo ' '

.PHONY: all clean
