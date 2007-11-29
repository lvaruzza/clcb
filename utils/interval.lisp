;;;; Copyright (c) 2007 Albert Krewinkel
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :clcb-utils)

;;; Some Set definitions
;;; --------------------
(defclass abstract-set () () (:documentation "base class for set operations"))

(defclass discrete-ordered-set (abstract-set)
  ((next-element-fn :accessor next-element-fn
                    :initarg :next-element-fn)
   (previous-element-fn :accessor previous-element-fn
                        :initarg :previous-element-fn)
   (element-member-fn :accessor element-member-fn
                      :initarg :element-member-fn))
  (:documentation "A list that cannot conain doublettes. The order is
given by the property of the objects, not by the order of insertion
into the set."))

(defgeneric next-element-in-set (set element)
  (:documentation "The smallest element x in set, for which element < x.")
  (:method (set element)
    (funcall (next-element-fn set) element)))

(defgeneric previous-element-in-set (set element)
  (:documentation "The biggest element x in set, for which x < element.")
  (:method (set element)
    (funcall (previous-element-fn set) element)))

(defgeneric element-member-p (set element)
  (:documentation "Return T iff the element is in the set.")
  (:method (set element) (funcall (element-member-fn set) set element)))


;;; Interval Class definitions
;;; --------------------------
(defclass interval (discrete-ordered-set) ()
  (:documentation "Abstract interval class.

An interval is a subset of a set with an total order defined on
it.  The interval is defined by two values x,y and contains all
numbers z so that x <= z <= y.  '<' can be used instead of <=

http://en.wikipedia.org/wiki/Interval_%28mathematics%29
"))

(defconstant +empty-interval+ '+empty-interval+)
;(defconstant +empty-interval+ (make-instance 'interval))


;;; Fundamental interval methods
;;; ----------------------------
(defgeneric lower-bound-included-p (interval)
  (:documentation "Return t iff the lower bound is part of the
  interval, nil otherwise."))
(defgeneric upper-bound-included-p (interval)
  (:documentation "Return t iff the upper bound is part of the
  interval, nil otherwise."))
(defgeneric lower-bound (interval)
  (:documentation "Returns the lower bound of the interval."))
(defgeneric upper-bound (interval)
  (:documentation "Returns the upper bound of the interval."))
(defgeneric interval-superset (interval)
  (:documentation "The set the interval is defined on."))


;;; ---------------------
;;; Some interval classes
;;; ---------------------

(defclass elementary-interval (interval) ())

(defmethod lower-bound-included-p ((i elementary-interval))
  (declare (ignore i)) t)
(defmethod upper-bound-included-p ((i elementary-interval))
  (declare (ignore i)) t)

(defclass number-interval (elementary-interval)
 ((lower-bound :accessor lower-bound
               :initarg :lower-bound)
  (upper-bound :accessor upper-bound
               :initarg :upper-bound)))

(defclass integer-interval (number-interval)
  ((lower-bound :type integer)
   (upper-bound :type integer)
   (next-element-fn :allocation :class
                    :initform #'1+)
   (previous-element-fn :allocation :class
                        :initform #'1-)
   (element-member-fn :initform #'(lambda (interval element)
                                    (and (integerp element)
                                         (<= (lower-bound interval)
                                             element
                                             (upper-bound interval)))))))


;;; -------------------------------------
;;; Multi Interval (= a set of intervals)
(defclass multi-interval (interval)
  ((intervals :accessor intervals
              :initarg :intervals
              :type list)))

(defmethod lower-bound ((mi multi-interval))
  (lower-bound (car (intervals mi))))
(defmethod upper-bound ((mi multi-interval))
  (upper-bound (car (last (intervals mi)))))

(defmethod element-member-p ((mi multi-interval) element)
  (some (rcurry #'element-member-p element) (intervals mi)))

(defmethod next-element-in-set ((mi multi-interval) element)
  (iter (generate interval in (intervals mi))
        (when (<= element (upper-bound (next interval)))
            (if (< element (upper-bound interval))
                (return (next-element-in-set interval element))
                (return (lower-bound (next interval)))))))

(defmethod previous-element-in-set ((mi multi-interval) element)
  (iter (generate interval in (reverse (intervals mi)))
        (when (<= (lower-bound (next interval)) element)
          (if (= element (lower-bound interval))
              (return (upper-bound (next interval)))
              (return (previous-element-in-set interval element))))))


;;; ------------------------------------
;;; Generic Interval methods
(defun interval-p (x) (typep x 'interval))

(defun interval-empty-p (interval) (eql interval +empty-interval+))

(defun singleton-p (interval)
  (= (lower-bound interval) (upper-bound interval)))

(defun convex-hull (interval)
  "The smalles interval that encloses the given set of elements."
  (make-interval 
   (if (typep interval 'multi-interval)
       (first (intervals interval))
       interval)
   (lower-bound interval) (upper-bound interval)))

(defmethod print-object ((ei (eql +empty-interval+)) (stream stream))
  (print '+empty-interval+ stream))

(defgeneric interval-elements (interval)
  (:documentation "The number of elements in the interval.")
  ;; assuming an integer interval by default seems reasonable.
  (:method (interval) (- (upper-bound interval) (lower-bound interval) -1))
  (:method ((ei (eql +empty-interval+))) 0)
  (:method ((mi multi-interval))
    (reduce #'+ (intervals mi):key #'interval-elements)))

(defgeneric interval-nth-element (interval n)
  (:documentation "Return the n-th element in the interval.")

  (:method (interval (n integer))
    (iter (initially (setf i (lower-bound interval)))
          (repeat (1- n))
          (for i = (next-element-in-set interval i))
          (finally 
           (return (if (funcall (element-member-fn interval) interval i)
                       i
                       nil)))))

  (:method ((mi multi-interval) (n integer))
    (iter (for interval in (intervals mi))
          (for num-elements = (interval-elements interval))
          (if (< num-elements n)
              (setf n (- n num-elements))
              (return (interval-nth-element interval n))))))


(defmethod print-object ((int interval) (stream stream))
  (print-unreadable-object (int stream :type t)
    (format stream "~:[]~;[~]~A,~A~:[[~;]~]"
            (lower-bound-included-p int)
            (lower-bound int)
            (upper-bound int)
            (upper-bound-included-p int))))

(defmethod print-object ((multint multi-interval) (stream stream))
  (print-unreadable-object (multint stream :type t)
    (format stream "(~{~A~^ ~})"
            (intervals multint))))

;;;; --------------------------------------------------------------------------
;;;; Interval instantiation

(defgeneric make-interval (interval-class lower upper &rest other-args)
  (:documentation "Create a new interval of type `interval-class'."))

;; Ensure that the lower bound is <= upper bound. I'm not sure if it's
;; a good idea to quietly return an empty-interval here, since this
;; isn't standard use of an interval and could be an error. On the
;; other hand, I do rely on this behavior to easily do interval
;; operations. But it will break code if we try to define intervals on
;; circular sets. (Sounds like a job for the condition systems and
;; restarts.)
(defmethod make-interval :around (interval lower upper &rest args)
  (declare (ignore args interval))
  (if (< upper lower)
      +empty-interval+
      (call-next-method)))

;;; -------------------------

;; We allow three method to specify the type of a new interval: 
;; If a class is supplied, the new symbol will be of the same class.
;; If a symbol is supplied, we use the class specified by the symbol.
;; If an object is given, the new interval is of the same interval type
;; as the object

(defmethod make-interval ((interval standard-object) lower upper &rest args)
  (declare (ignore args))
  (let ((new-interval (moptilities:copy-template interval)))
    (setf (lower-bound new-interval) lower
          (upper-bound new-interval) upper)
    new-interval))

(defmethod make-interval ((interval-class symbol) lower upper &rest args)
  (apply #'make-interval (find-class interval-class) lower upper args))




;;; ------------------------------------
;;; Interval comparison
(defmethod lower-bound-= (i1 i2) (= (lower-bound i1) (lower-bound i2)))
(defmethod lower-bound-< (i1 i2) (< (lower-bound i1) (lower-bound i2)))
(defmethod lower-bound-> (i1 i2) (> (lower-bound i1) (lower-bound i2)))
(defmethod upper-bound-= (i1 i2) (= (upper-bound i1) (upper-bound i2)))
(defmethod upper-bound-< (i1 i2) (< (upper-bound i1) (upper-bound i2)))
(defmethod upper-bound-> (i1 i2) (> (upper-bound i1) (upper-bound i2)))
(defmethod lower-bound-<= (i1 i2)
  (or (lower-bound-< i1 i2) (lower-bound-= i1 i2)))
(defmethod upper-bound-<= (i1 i2)
  (or (upper-bound-< i1 i2) (upper-bound-= i1 i2)))


;;; ------------------------------------
;;; Interval group operations
;;; ------------------------------------

;; Interval group operations are commutative, but we don't want to
;; write each method twice if it specializes on different classes.
(defmacro def-commutative-method (name lambda-list &body body)
  `(progn (defmethod ,name ,lambda-list ,@body)
          (defmethod ,name (,(cadr lambda-list) ,(car lambda-list)) ,@body)))

(defun lower-bound-max (i1 i2) (if (lower-bound-< i1 i2) i2 i1))
(defun lower-bound-min (i1 i2) (if (lower-bound-> i1 i2) i2 i1))
(defun upper-bound-max (i1 i2) (if (upper-bound-< i1 i2) i2 i1))
(defun upper-bound-min (i1 i2) (if (upper-bound-> i1 i2) i2 i1))

;; -------------
;; Intersections
(defgeneric interval-intersection (interval1 interval2)
  (:documentation "Intersection of two intervals.")

  (:method (i1 i2)
    (make-interval i1 (lower-bound (lower-bound-max i1 i2))
                   (upper-bound (upper-bound-min i1 i2)))))

(def-commutative-method interval-intersection (si (mi multi-interval))
  (reduce #'interval-union
          (mapcar #'(lambda (x) (interval-intersection si x))
                  (intervals mi))))

(def-commutative-method interval-intersection (i1 (i2 (eql +empty-interval+)))
  (declare (ignore i1 i2))
  +empty-interval+)


;; -----
;; Union 
(defgeneric interval-union (interval1 interval2)
  (:documentation "Build the union of two intervals.  If the
  intervals are overlapping, a new interval with the type of
  interval1 is returned. Otherwise, the intervals are combined to
  a multi-interval."))

(defmethod interval-union (i1 i2)
  (let ((lower (lower-bound-min i1 i2))
        (upper (upper-bound-max i1 i2)))
    (if (interval-empty-p (interval-intersection lower upper))
        (make-instance 'multi-interval :intervals (list lower upper))
        (make-interval i1 (lower-bound lower) (upper-bound upper)))))

;; if one of the intervals is empty, we can simply return the other
(def-commutative-method interval-union ((i1 (eql +empty-interval+)) i2)
  i2)

(def-commutative-method interval-union ((i1 multi-interval) i2)
  (make-instance 'multi-interval
                 :intervals (cons i2 (copy-list (intervals i1)))))

(defmethod interval-union ((i1 multi-interval) (i2 multi-interval))
  (let ((res (make-instance 'multi-interval :intervals
                            (nconc (copy-list (intervals i1))
                                   (copy-list (intervals i2))))))
    (if (null (cdr (intervals res)))
        (car (intervals res))
        res)))


;;; Complement
;;; ------------------------------------
(defgeneric interval-complement (interval complement-in)
  (:method (interval (ei (eql +empty-interval+))) ei)
  (:method ((ei (eql +empty-interval+)) complement-in) complement-in)
  (:method (interval complement-in)
   (interval-union
    (make-interval interval
                   (lower-bound complement-in)
                   (previous-element-in-set complement-in
                                            (lower-bound interval)))
    (make-interval interval
                   (next-element-in-set interval
                                        (upper-bound interval))
                   (upper-bound complement-in)))))

(defgeneric interval-relative-complement (interval complement-in)
  (:documentation "Complement of interval relative to a superset-interval."))

(defmethod interval-relative-complement (interval complement-in)
  (interval-complement (interval-intersection interval complement-in)
                       complement-in))

(defmethod interval-relative-complement ((mi multi-interval) complement-in)
  (reduce #'interval-intersection
          (mapcar #'(lambda (x) (interval-relative-complement x complement-in))
                  (intervals mi))
          :initial-value complement-in))

(defmethod interval-relative-complement (interval (mi multi-interval))
  (reduce #'interval-union
          (mapcar #'(lambda (x) (interval-relative-complement interval x))
                  (intervals mi))))



;;; ------------------------------------
;;; Multi-Interval support
;;; ------------------------------------
(defun merge-sorted-intervals (intervals)
  (let* ((intervals (remove-if #'interval-empty-p intervals))
         (res (list (elt intervals 0))))
    (iter (for interval in (subseq intervals 1))
          ; FIXME: incorrect for open intervals
          (if (<= (lower-bound interval) (upper-bound (car res)))
              (setf (car res) (interval-union (car res) interval))
              (push interval res)))
    (nreverse res)))

(defun merge-intervals (intervals)
  (merge-sorted-intervals (sort (remove-if #'interval-empty-p intervals)
                                #'lower-bound-<)))

(defmethod initialize-instance :after ((mi multi-interval) &key)
  (setf (slot-value mi 'intervals)
        (ensure-list (merge-intervals  (slot-value mi 'intervals)))))



(defparameter *integer-set*
  (make-instance 'discrete-ordered-set
                 :next-element-fn #'1+
                 :previous-element-fn #'1-
                 :element-member-fn #'integerp))



#||(defmethod interval-superset ((intev integer-interval))
  (declare (ignore intev))
  *integer-set*)||#

(defmethod make-interval ((class (eql (find-class 'integer-interval)))
                          (lower integer) (upper integer) &rest other-args)
  (declare (ignore other-args))
  (make-instance 'integer-interval :lower-bound lower :upper-bound upper))


(defun parse-interval-expression (stream subchar min-args)
  (declare (ignore char))
  (let ((lower (read stream t nil))
        (comma (read-char stream t nil))
        (upper (read stream t nil)))
    `(make-interval 'integer-interval ,lower ,upper)))

(set-dispatch-macro-character #\# #\i #'parse-interval-expression)



;;;; -------------------------------------------------------------------------
;;;; TESTS!!!!
;;;; -------------------------------------------------------------------------
(defparameter *interval1* 
  (make-interval 'integer-interval 0 100 t nil))

(defparameter *interval2*
  (make-interval 'integer-interval 23 42 t t))

(defparameter *multi-interval*
  (make-instance 'multi-interval :intervals
                 (mapcar #'(lambda (start end) (make-interval
                                                'integer-interval
                                                start end))
                         '(5  50 70 65)
                         '(13 60 90 70))))
