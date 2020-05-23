# FFT Program:

import matplotlib.pyplot as plt
import numpy as np
# Learn about API authentication here: https://plot.ly/python/getting-started
# Find your api_key here: https://plot.ly/settings/api

Fs = 44100.0;  # sampling rate
Ts = 1.0/Fs; # sampling interval
t = np.arange(0,3,Ts) # time vector
print("Time Vector: {0}". format(len(t)))

ff = 3500;   # frequency of the signal
y = np.sin(2*np.pi*ff*t)

n = len(y) # length of the signal
print("Length of FFT: {0}". format(n))
k = np.arange(n)
print("k: {0}". format(k))
T = n/Fs
print("T: {0}". format(T))
frq = k/T # two sides frequency range
print("Frq (k/T): {0}". format(frq))
frq = frq[range(n/2)] # one side frequency range

Y = np.fft.fft(y)/n # fft computing and normalization
Y = Y[range(n/2)]

fig, ax = plt.subplots(2, 1)
ax[0].plot(t,y)
ax[0].set_xlabel('Time')
ax[0].set_ylabel('Amplitude')
ax[1].plot(frq,abs(Y),'r') # plotting the spectrum
ax[1].set_xlabel('Freq (Hz)')
ax[1].set_ylabel('|Y(freq)|')

plt.show()
