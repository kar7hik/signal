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
                #:cl-portaudio
                #:vgplot
                ;#:bordeaux-fft
                #:napa-fft3
                #:array-operations)

  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "global")
                             (:file "general-error")
                             (:file "utils")
                             (:module signal-processing
                              :serial t
                              :components ((:file "signal-processing")
                                           (:file "signal-utils")))
                             (:module audio-io
                              :serial t
                              :components ((:file "io-utils")
                                           (:file "audio-io")
                                           (:file "wav")
                                           (:file "feature-extraction")))
                             (:module core
                              :serial t
                              :components ((:file "signal")))
))))
