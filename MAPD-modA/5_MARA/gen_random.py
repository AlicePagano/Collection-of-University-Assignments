from random import randrange
import numpy as np
import matplotlib.pyplot as plt

file1 = open("input.txt","w")

nsamples = 1000
fs=500
frequency=1
x = np.arange(nsamples)
signal1 = np.sin(2 * np.pi * frequency * x / fs)

for i in signal1:
    p = np.int(i * np.power(2,8) + np.power(2,8) )
    file1.write(str(p))
    file1.write("\n")

file1.close()

print(signal1)
#plt.plot(signal1)
#plt.show()
