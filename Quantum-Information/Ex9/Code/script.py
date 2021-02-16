#!/usr/bin/env python
# coding: utf-8

# Load packages
import numpy as np
import subprocess
import sys
import os

# Input variables
Nmin  = 3
Nmax  = 10 #10
Nstep = 1

lambda_min = 0
lambda_max = 3
lambda_step = 0.1

klevels = 7

# Data
N_list = np.arange(Nmin,Nmax+Nstep,Nstep,dtype=int)

lambda_list = np.arange(lambda_min,lambda_max+lambda_step,lambda_step,dtype=float)

print('N range     :', N_list,'\n')
print('Lambda range:',lambda_list,'\n')

# Define executable and program name
exe = "ising_hamilt"
program = "ising_hamilt.f90"

# Clean executable
command = ["rm","-f",exe]
make_proc = subprocess.run(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

# Clean output files
os.system('rm -rf ./results/eig_*.dat')
os.system('rm -rf ./results_new/new_*.dat')
os.system('rm -rf ./plots/eig_*.pdf')
os.system('rm -rf ./plots_new/new_*.pdf')

# Compile program
command = ["gfortran","-o",exe,program,"-L/usr/local/opt/lapack/lib","-llapack"]
make_proc = subprocess.run(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

if (make_proc.stderr):
    print('Stderr :', result.stderr)

for N in N_list:
    for lam in lambda_list:
        result = subprocess.run(['./'+exe,str(N),str(round(lam,2)),'eig_'+str(N)+'.dat',str(klevels)],stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,encoding="utf-8")
        print(result.stdout)

        if (result.stderr):
            print('Stderr :', result.stderr)

if not os.path.isdir('./plots/'):
    os.mkdir('./plots/')

file_name = ['eig_'+str(i) for i in N_list]

print(file_name)
print(str(klevels))

# Plot data with gnuplot
for file in file_name:
    result = subprocess.run(['gnuplot','-e',"k="+str(klevels),'-e', "file_name='"+file,'plot.p'],
                            stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

    if (result.stdout):
        print('Stdout :', result.stdout,'\n')
    if (result.stderr):
        print('Stderr :', result.stderr,'\n')


# New
if not os.path.isdir('./plots_new/'):
    os.mkdir('./plots_new/')

new_list = [0.0,0.5,1.0,1.5,2.0,2.5,3.0]
file_name_new = ['new_'+str(round(j,2))+'_eig_'+str(i) for j in new_list for i in N_list]

# Plot data with gnuplot
for file in file_name_new:
    result = subprocess.run(['gnuplot','-e', "file_name='"+file,'new_plot.p'],
                            stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

    if (result.stdout):
        print('Stdout :', result.stdout,'\n')
    if (result.stderr):
        print('Stderr :', result.stderr,'\n')
