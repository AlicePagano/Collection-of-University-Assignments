#!/usr/bin/env python
# coding: utf-8

# Load packages
import numpy as np
import subprocess
import sys
import os

exe = "hermitian"
program = "hermitian.f90"

# Clean executable
command = ["rm","-f",exe]
make_proc = subprocess.run(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

# Compile program
command = ["gfortran","-o",exe,program,"-llapack"]
make_proc = subprocess.run(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

# Define variables
N = 1000
a = -5.0
b = 5.0

result = subprocess.run(['./'+exe,str(N),str(a),str(b)],stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,encoding="utf-8")
print(result.stdout)

if (result.stderr):
    print('Stderr :', result.stderr)


if not os.path.isdir('./plots/'):
    os.mkdir('./plots/')

# Fix maximum eigenfunciton to plot
Nmax = 8

# Plot eigenfunctions
for i in range(1,Nmax+1):
    result = subprocess.run(['gnuplot','-e',"col="+str(i+1),'plot_eig_func.p'],stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE,encoding="utf-8")
    if (result.stdout):
        print('Stdout :', result.stdout,'\n')
    if (result.stderr):
        print('Stderr :', result.stderr,'\n')


# Plot difference between numerical and analytical eigenvalues
result = subprocess.run(['gnuplot','plot_eig_val.p'],stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE,encoding="utf-8")
if (result.stdout):
    print('Stdout :', result.stdout,'\n')
if (result.stderr):
    print('Stderr :', result.stderr,'\n')
