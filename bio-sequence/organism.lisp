(in-package #:clcb)
(defclass organism ()
  ((name :accessor organism-name
         :initarg :name)
   (taxonomy :accessor organism-taxonomy
             :initarg :taxonomy))
   (:documentation "Name and taxonomic range of an organism. Such information is referenced by sequences and instrumental to know, e.g., for a set of orthologues that one is comparing."))
