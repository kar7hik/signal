;;;; package.lisp

(defpackage #:signal
  (:use #:cl
        #:iterate
        #:alexandria
        #:bordeaux-fft
        #:portaudio
        #:clgplot
        #:wav)
  (:shadowing-import-from #:iterate #:terminate #:in)
  (:export #:array-to-list))


(in-package #:signal)
