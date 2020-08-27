;;;; signal.asd
;;
;;;; Copyright (c) 2019 S. Karthik kumar <karthikkumar.s@protonmail.com>


(asdf:defsystem #:signal
  :description "Describe signal here"
  :author "S. Karthik kumar <karthikkumar.s@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:iterate
                #:alexandria
                #:petalisp
                #:lparallel
                #:cl-portaudio
                #:vgplot
                ;#:bordeaux-fft
                #:eazy-gnuplot
                #:napa-fft3
                #:array-operations
                #:dct
                #:random-state
                #:external-program
                )

  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "global")
                             (:file "general-error")
                             (:file "utils")
                             (:module plotting
                              :serial t
                              :components ((:file "gnuplot")
                                           (:file "plot")
                                           ))
                             (:module signal-processing
                              :serial t
                              :components ((:file "signal-processing")
                                           (:file "signal-utils")))
                             (:module audio-io
                              :serial t
                              :components ((:file "io-utils")
                                           (:file "feature-extraction-helper")
                                           (:file "audio-io")
                                           (:file "wav")
                                           (:file "mfcc-helper")
                                           (:file "feature-extraction")))
                             (:module self-organizing
                              :serial t
                              :components ((:file "som")))
                             (:module core
                              :serial t
                              :components ((:file "signal")))
))))
