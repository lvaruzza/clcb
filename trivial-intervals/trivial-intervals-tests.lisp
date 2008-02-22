(defpackage trivial-intervals-tests
  (:use #:common-lisp
        #:lift
        #:trivial-intervals
        ))

(in-package #:trivial-intervals-tests)

(deftestsuite interval-tests ()
  ())

(addtest (interval-tests)
  trivial-interval-reader-macro
  (ensure-same (read-from-string "#[3.1415,42]")
               (make-instance 'trivial-interval
                              :lower 3.1415
                              :upper 42)
               :test #'interval-=))

(addtest (interval-tests)
  integer-interval-reader-macro
  (ensure-same (read-from-string "#[23,42]")
               (make-instance 'integer-interval
                              :lower 23 :upper 42)
               :test #'interval-=))




