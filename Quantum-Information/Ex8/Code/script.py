#!/usr/bin/env python
# coding: utf-8

# Load packages
import numpy as np
import subprocess
import sys
import os

Nmin = 10000
Nmax = 1000000
Dmin = 2
Dmax = 5
# Data
N_list = np.arange(Nmin,Nmax+1,10000,dtype=int)
D_list = np.arange(Dmin,Dmax+1,dtype=int)
sep_list = ['T'] #['T','F']
print(N_list,'\n')
print(D_list,'\n')

exe = "density_matrix_init"
program = "density_matrix_init.f90"

# Clean executable
command = ["rm","-f",exe]
make_proc = subprocess.run(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

# Compile program
command = ["gfortran","-o",exe,program]
make_proc = subprocess.run(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

for sep in sep_list:
    for D in D_list:
        for N in N_list:
            result = subprocess.run(['./'+exe,str(N),str(D),sep,'time_'+sep+'_'+str(D)+'.dat'],stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,encoding="utf-8")
            print(result.stdout)

            if (result.stderr):
                print('Stderr :', result.stderr)
