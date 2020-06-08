;;;; package.lisp

(defpackage #:signal
  (:use #:cl
        #:iterate
        #:alexandria-2
        ;#:bordeaux-fft
        #:napa-fft
        #:portaudio
        #:vgplot)
  (:shadowing-import-from #:iterate #:terminate #:in)
  (:export #:array-to-list))


(in-package #:signal)
