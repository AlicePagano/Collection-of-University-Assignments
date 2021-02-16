#!/usr/bin/env python
# coding: utf-8

# Load packages
import numpy as np
import subprocess
import sys
import os

# Load data
input_file = 'input.dat'
input_data = np.loadtxt(input_file,dtype=int)

# Clean all
make_command = ["make","clean"]
make_proc = subprocess.run(make_command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

executable = ['mat_mult_row','mat_mult_col','mat_mult_mat']
optimization = ['O0','O1','O2','O3']

# Compile program for different optimization flags
for opt in optimization:
    make_command = ["make","all","FAST=-"+opt]
    make_proc = subprocess.run(make_command,stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE,encoding="utf-8")
    # Iterate for value of matrix size
    for n in input_data:
        # Call executables
        for exe in executable:
            subprocess.run(['./'+exe,str(n),'time_'+opt+'_'+exe+'.dat'],stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE,encoding="utf-8")

if not os.path.isdir('./plot'):
    os.mkdir('./plot')

# Plot fit for different optimization flags
for opt in optimization:
    subprocess.run(['gnuplot','-e',"opt_flag='"+opt+"'",'fit.p'],stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE,encoding="utf-8")
