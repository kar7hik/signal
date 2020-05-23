
(asdf:defsystem #:signal-test
  :name "signal-test"
  :description "Testing functions used in the signal package."
  :author "S. Karthik kumar"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:signal)

  :serial t
  :components ((:module "test"
                        :components ((:file "package")
                                     (:file "audio-test"
                                            :depends-on ("package"))))))



