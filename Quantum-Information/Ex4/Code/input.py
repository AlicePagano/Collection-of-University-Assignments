#!/usr/bin/env python
# coding: utf-8

# Load packages
import numpy as np

# CREATE INPUT DATA
N_min = 100
N_max = 300
N_step = 100

# Create list between N_min and N_max
#N_list = np.arange(N_min,N_max+N_step,N_step)
N_list = np.logspace(2, 3.1763, num=12,dtype=int)

# Save list in a file
input_file = 'input.dat'
np.savetxt(input_file, N_list,fmt='%8i')
