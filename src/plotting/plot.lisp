(in-package #:signal)

;; (defun simple-plot-signal (x-sequence &key (y-sequence nil y-sequence-supplied-p)
;;                                         (output "filename.png" output-supplied-p)
;;                                         (title "Auto-generated")
;;                                         (x-label "x-label")
;;                                         (y-label "y-label"))
;;   (eazy-gnuplot:with-plots (*standard-output* :debug nil)
;;     (if output-supplied-p
;;         (eazy-gnuplot:gp-setup :terminal '(pngcairo)
;;                                :output output
;;                                :title title
;;                                :xlabel x-label
;;                                :ylabel y-label)
;;         (eazy-gnuplot:gp-setup :terminal '(qt)
;;                                :title title
;;                                :xlabel x-label
;;                                :ylabel y-label))
;;     (when y-sequence-supplied-p
;;       (eazy-gnuplot:plot (lambda ()
;;                            (iter
;;                              (for p in (map 'list (lambda (x y) (list x y))
;;                                             x-sequence
;;                                             y-sequence))
;;                              (format t "~&~{~a~^ ~}" p)))
;;                          :with '(lines linestyle 1)))
;;     (unless y-sequence-supplied-p
;;       (eazy-gnuplot:plot (lambda ()
;;                            (iter
;;                              (for i :in-sequence x-sequence)
;;                              (format t "~&~A" i)))
;;                          :with '(lines linestyle 1)))
;;     (format t "~&pause mouse button1;~%"))
;;   (when output-supplied-p
;;     output))



(defun plot-signal (x filename &key (y nil y-supplied-p)
                                 (signal-label ";signal;")
                                 (x-label "x-label")
                                 (y-label "y-label")
                                 (linespace '(0 1000 0 100) linespace-supplied-p)
                                 (title "Auto-generated"))
  "Minimal high level function to interact with VGPLOT."
  (progn
    (if y-supplied-p
        (vgplot:plot x y signal-label)
        (vgplot:plot x signal-label))
    (vgplot:xlabel x-label)
    (vgplot:ylabel y-label)
    (vgplot:title title)
    (when linespace-supplied-p
      (vgplot:axis linespace))
    (vgplot:print-plot (create-result-file-path filename))
    (vgplot:format-plot t "set terminal ~A" "qt"))
  'SAVED)
