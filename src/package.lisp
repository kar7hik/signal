;;;; package.lisp

(defpackage #:signal
  (:use #:cl
        #:iterate
        #:alexandria
        #:bordeaux-fft
        #:portaudio
        #:clgplot)
  (:shadowing-import-from #:iterate #:terminate #:in)
  (:export #:array-to-list))


(in-package #:signal)
