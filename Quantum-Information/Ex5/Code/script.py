#!/usr/bin/env python
# coding: utf-8

# Load packages
import numpy as np
import subprocess
import sys
import os

# Clean all
make_command = ["make","clean"]
make_proc = subprocess.run(make_command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,encoding="utf-8")

executable = ['hermitian','diagonal']

# Compile programs
make_command = ["make","all"]
make_proc = subprocess.run(make_command,stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,encoding="utf-8")

# Define variables
N      = 5000
Ntime  = 10
Nbins  = 100 #int(np.log(N*Ntime))

level = [10,50,100,250,1000]

for lev in level:
    for exe in executable:
        print("Type  : ", exe)
        print("N     : ", N)
        print("Nbins : ", Nbins)
        print("Level : ", lev)
        print("Ntime : ", Ntime,'\n')

        result = subprocess.run(['./'+exe,str(N),str(Nbins),str(Ntime),str(lev)],stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,encoding="utf-8")
        print(result.stdout)
        if (result.stderr):
            print('Stderr :', result.stderr)

if not os.path.isdir('./hist_plot_pro/'):
    os.mkdir('./hist_plot_pro/')

# Plot histogram
for exe in executable:
    result = subprocess.run(['gnuplot','-e',"file_name='"+exe,'plot_hist_pro.p'],stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE,encoding="utf-8")
    if (result.stdout):
        print('Stdout :', result.stdout,'\n')
    if (result.stderr):
        print('Stderr :', result.stderr,'\n')
