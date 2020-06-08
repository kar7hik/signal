import os
import numpy as np
import scipy
from scipy.io import wavfile
import scipy.fftpack as fft
from scipy.signal import get_window
from scipy.fftpack import dct
import matplotlib.pyplot as plt


filename = '/home/karthik/quicklisp/local-projects/signal/track-1.wav'
sample_rate, audio = scipy.io.wavfile.read(filename)


print("Sample rate: {0}Hz".format(sample_rate))
print("Audio duration: {0}s".format(len(audio) / sample_rate))


def normalize_audio(audio):
    audio = audio / np.max(np.abs(audio))
    return audio


# audio = normalize_audio(audio)
# plt.figure(figsize=(15, 4))
# plt.plot(np.linspace(0, len(audio) / sample_rate, num=len(audio)), audio)
# plt.grid(True)
# plt.show()


def frame_audio(audio, FFT_size=2048, hop_size=10, sample_rate=44100):
    # hop_size in ms
    audio = np.pad(audio, int(FFT_size / 2), mode='reflect')
    frame_len = np.round(sample_rate * hop_size / 1000).astype(int)
    frame_num = int((len(audio) - FFT_size) / frame_len) + 1
    frames = np.zeros((frame_num, FFT_size))
    print("audio shape: {0}".format(audio.shape))
    print("Frame len: {0}".format(frame_len))
    print("Frame num: {0}".format(frame_num))
    print("Frames shape: {0}".format(frames.shape))

    for n in range(frame_num):
        frames[n] = audio[n*frame_len:n*frame_len+FFT_size]
    return frames


hop_size = 10  # ms
FFT_size = 2048
audio_framed = frame_audio(audio, FFT_size=FFT_size,
                            hop_size=hop_size, sample_rate=sample_rate)
# print("Framed audio shape: {0}".format(audio_framed.shape))
