import numpy
import scipy.io.wavfile
from scipy.fftpack import dct
import matplotlib.pyplot as plt
import os

def write_data_to_file(data, filename):
    dir = '/home/karthik/quicklisp/local-projects/signal/'
    f = open(os.path.join(dir, filename), 'w+')
    for i in range(len(data)):
        f.write(str(data[i]) + "\n")
    f.close()


def read_data_from_file(filename):
    dir = '/home/karthik/quicklisp/local-projects/signal/'
    f = open(os.path.join(dir, filename))  # Open file on read mode
    lines = f.read().split("\n")  # Create a list containing all lines
    f.close()  # Close file
    return lines


filename = '/home/karthik/quicklisp/local-projects/signal/low-freq.wav'
sample_rate, signal = scipy.io.wavfile.read(filename)
signal = signal[0:int(1.0 * sample_rate)]  # Keep the first 3.5 seconds

pre_emphasis = 0.97
emphasized_signal = numpy.append(signal[0], signal[1:] - pre_emphasis * signal[:-1])


frame_size = 0.020
frame_stride = 0.01

frame_length, frame_step = frame_size * sample_rate, frame_stride * sample_rate  # Convert from seconds to samples
signal_length = len(emphasized_signal)
frame_length = int(round(frame_length))
frame_step = int(round(frame_step))
num_frames = int(numpy.ceil(float(numpy.abs(signal_length - frame_length)) / frame_step))  # Make sure that we have at least 1 frame

pad_signal_length = num_frames * frame_step + frame_length
z = numpy.zeros((pad_signal_length - signal_length))
pad_signal = numpy.append(emphasized_signal, z) # Pad Signal to make sure that all frames have equal number of samples without truncating any samples from the original signal

indices = numpy.tile(numpy.arange(0, frame_length), (num_frames, 1)) + numpy.tile(numpy.arange(0, num_frames * frame_step, frame_step), (frame_length, 1)).T
frames = pad_signal[indices.astype(numpy.int32, copy=False)]
# print(len(frames))

frames *= numpy.hamming(frame_length)

NFFT = 1024
mag_frames = numpy.absolute(numpy.fft.rfft(frames, NFFT))  # Magnitude of the FFT
pow_frames = ((1.0 / NFFT) * ((mag_frames) ** 2))

print("mag_frames shape: {}".format(numpy.shape(mag_frames)))
# write_data_to_file(mag_frames[56], 'result.txt')
# plt.plot(pow_frames[2])
# plt.show()

# plt.plot(frames[2])
# plt.show()

# reverse = numpy.absolute(numpy.fft.ifft(mag_frames, NFFT))
# plt.plot(reverse[2])
# plt.show()
nfilt = 40
low_freq_mel = 0
high_freq_mel = (2595 * numpy.log10(1 + (sample_rate / 2) / 700))  # Hz to Mel
mel_points = numpy.linspace(low_freq_mel, high_freq_mel, nfilt + 2)  # Equally spaced in Mel scale
hz_points = (700 * (10**(mel_points / 2595) - 1))  # Convert Mel to Hz
bin = numpy.floor((NFFT + 1) * hz_points / sample_rate)

fbank = numpy.zeros((nfilt, int(numpy.floor(NFFT / 2 + 1))))
print("fbank shape: {}".format(numpy.shape(fbank)))
print("pow_frames shape: {}".format(numpy.shape(pow_frames)))

for m in range(1, nfilt + 1):
    f_m_minus = int(bin[m - 1])   # left
    f_m = int(bin[m])             # center
    f_m_plus = int(bin[m + 1])    # right

    for k in range(f_m_minus, f_m):
        fbank[m - 1, k] = (k - bin[m - 1]) / (bin[m] - bin[m - 1])
    for k in range(f_m, f_m_plus):
        fbank[m - 1, k] = (bin[m + 1] - k) / (bin[m + 1] - bin[m])


# write_data_to_file(fbank, 'fbank.txt')
filter_banks = numpy.dot(pow_frames, fbank.T)
print("filter_banks shape: {}".format(numpy.shape(filter_banks)))
filter_banks = numpy.where(filter_banks == 0, numpy.finfo(float).eps, filter_banks)  # Numerical Stability
# write_data_to_file(filter_banks, 'filter.txt')

filter_banks = 20 * numpy.log10(filter_banks)  # dB

print("hz_points: {}".format(hz_points))
print("mel_points: {}".format(mel_points))

print("hz_points_len: {}".format(len(hz_points)))
print("mel_points_len: {}".format(len(mel_points)))


# print(sample_rate)
print("bin: {}".format(bin))
print("filter_banks: {}".format(filter_banks))
freq_min = 0
freq_high = sample_rate / 2
mel_filter_num = 10

print("Minimum frequency: {0}".format(freq_min))
print("Maximum frequency: {0}".format(freq_high))
