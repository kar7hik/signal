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
                #:clgplot
                #:bordeaux-fft
                #:cl-wav)

  :serial t
  :components ((:file "quickutils")
               (:module "src"
                :components ((:file "package")
                             (:file "global")
                             (:file "general-error")
                             (:file "utils")
                             (:module audio-io
                              :serial t
                              :components ((:file "io-utils")
                                           (:file "audio-io")
                                           (:file "feature-extraction")
                                           (:file "wav")))
                             (:module core
                              :serial t
                              :components ((:file "signal")))
                             (:module signal-processing
                              :serial t
                              :components ((:file "signal-processing")
                                           (:file "signal-utils")))))))
