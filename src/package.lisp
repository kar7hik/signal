;;;; package.lisp

(defpackage #:signal
  (:use #:cl
        #:iterate
        #:alexandria-2
        #:petalisp
        #:lparallel
        ;#:bordeaux-fft
        #:napa-fft
        #:portaudio
        #:vgplot)
  (:shadowing-import-from #:iterate #:terminate #:in)
  (:shadowing-import-from #:alexandria-2 #:flatten)
  (:shadowing-import-from #:vgplot #:range)

  (:export #:array-to-list))


(in-package #:signal)
