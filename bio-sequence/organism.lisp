(in-package #:clcb)
(defclass organism ()
  ((name :accessor organism-name
         :initarg :name)
   (taxonomy :accessor organism-taxonomy
             :initarg :taxonomy)))
