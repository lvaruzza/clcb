(defpackage clcb-tests
  (:use #:common-lisp
        #:trivial-intervals
        #:lift))

(in-package #:clcb-tests)

(deftestsuite interval-tests () ())

(addtest (interval-tests)
  trivial-interval-reader-macro
  (ensure-same #[3.1415,42]
               (make-instance 'interval:trivial-interval
                              :lower 3.1415
                              :upper 42)
               :test #'interval-=))

(addtest (interval-tests)
  integer-interval-reader-macro
  (ensure-same #[23,42]
               (make-instance 'interval:integer-interval :lower 23 :upper 42)
               :test #'interval-=))




