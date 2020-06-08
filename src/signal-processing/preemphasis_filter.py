import os
import numpy
import scipy.io.wavfile

sample_rate, signal = scipy.io.wavfile.read('/home/karthik/quicklisp/local-projects/signal/new-mic.wav')  # File assumed to be in the same directory
signal = signal[0:int(0.0004 * sample_rate)]  # Keep the first 3.5 seconds


def write_data_to_file(data, filename):
    dir = '/home/karthik/quicklisp/local-projects/signal/'
    f = open(os.path.join(dir, filename), 'w+')
    for i in range(len(data)):
        f.write(str(data[i]) + "\n")

    f.close()


def preemphasis(signal, coeff=0.95):
    """perform preemphasis on the input signal.
    :param signal: The signal to filter.
    :param coeff: The preemphasis coefficient. 0 is no filter, default is 0.95.
    :returns: the filtered signal.
    """
    return numpy.append(signal[0], signal[1:] - coeff * signal[:-1])


result = preemphasis(signal)
# write_data_to_file(signal, 'signal.txt')
# write_data_to_file(result, 'preemphasis.txt')
# print(type(signal))
# print(signal)
# print(result)
# preemphasis()
