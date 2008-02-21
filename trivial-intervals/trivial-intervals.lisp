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

(defpackage #:trivial-intervals
  (:use #:common-lisp
        #:iterate)
  (:import-from #:moptilities copy-template)
  (:export ;; Interval API
           #:lower-number
           #:upper-number
           #:interval-length
           #:make-interval

           ;; Important Classes
           #:abstract-interval
           #:trivial-interval
           #:integer-interval
           #:abstract-multi-interval
           #:multi-interval

           ;; Basic functions
           #:interval-empty-p
           #:singleton-p
           #:interval-=

           ;; Set theoretic methods
           #:interval-intersection
           #:interval-union
           ))


(in-package :trivial-intervals)

;;;; =========================================================================
;;;; METHODS ON INTERVALS
;;;; =========================================================================
;;; Fundamental interval methods.  All classes whose objects will be
;;; used as intervalls should support these.
(defgeneric lower-number (interval)
  (:documentation "Returns the lower bound of the interval."))
(defgeneric upper-number (interval)
  (:documentation "Returns the upper bound of the interval."))
(defgeneric interval-length (interval)
  (:documentation "Return the length of an interval (the measure, from
  a mathematical point of view)."))

(defgeneric make-interval (template lower upper
                                    &rest args &key &allow-other-keys)
  (:documentation "Create a new interval with bounds `lower' and
  `upper'.  If `template' is a class or a symbol naming a class, the
  new interval is of this type.  In case `template' is an object, the
  new object is created by copying template and setting the lower and
  upper value afterwards.  This method exists to allow for an unified
  way of instantiating intervals. ")
  (:method ((class-name symbol) lower upper &rest args)
    (apply #'make-interval (find-class class-name) lower upper args))
  (:method ((class standard-class) lower upper &rest args)
    (apply #'make-instance class :lower lower :upper upper args))
  (:method ((obj standard-object) lower upper &rest args)
    (declare (ignore args))
    (let ((new-obj (copy-template obj)))
      (setf (lower-number new-obj) lower)
      (setf (upper-number new-obj) upper)
      new-obj)))


;;;; =========================================================================
;;;; Some Interval Classes
;;;; =========================================================================
(defclass abstract-interval () ()
  (:documentation "Mixin interval class."))

;;; An interval has two boundaries which are inclusive.
(defclass trivial-interval (abstract-interval)
  ((lower :accessor lower-number
          :initarg :lower)
   (upper :accessor upper-number
          :initarg :upper))
  (:documentation "Very basic interval class."))

(defclass integer-interval (trivial-interval)
  ((lower :type integer)
   (upper :type integer)))

(defmethod interval-length ((itv trivial-interval))
  (1+ (- (upper-number itv) (lower-number itv))))

;;; ==============================================
;;; Basic Interval Arithmetic
;;; ==============================================

;; Most interval operations are commutative, but we don't want to
;; write each method twice if it specializes on different classes.
(defmacro def-commutative-method (name lambda-list &body body)
  `(progn (defmethod ,name ,lambda-list ,@body)
          (defmethod ,name (,(cadr lambda-list) ,(car lambda-list)) ,@body)))

(defgeneric sub2 (x y)
  (:documentation "Substract y from x."))

(defgeneric mult2 (x y)
  (:documentation "Multiply x and y."))

(defgeneric div2 (x y)
  (:documentation "Divide x by y."))

(defgeneric add2 (x y)
  (:documentation "Calculate the sum of x and y.")
  
  (:method ((x trivial-interval)
            (y trivial-interval))
    (make-instance (class-of x)
                   :lower (+ (lower-number x) (lower-number y))
                   :upper (+ (upper-number x) (lower-number y)))))

;; Number operations
;; FIXME: Multiple evaluations possible
(defmacro interval-real-op (interval-type fn-name fn &optional inverse-bounds)
  `(progn
     (defmethod ,fn-name ((x ,interval-type) (i real))
       (make-instance (class-of x)
                      :lower (,fn (lower-number x) i)
                      :upper (,fn (upper-number x) i)))
     (defmethod ,fn-name ((i real) (x ,interval-type))
       (make-instance 
        (class-of x)
        ,(if inverse-bounds :upper :lower) (,fn i (lower-number x))
        ,(if inverse-bounds :lower :upper) (,fn i (upper-number x))))
     (defmethod ,fn-name ((x real) (y real))
       (,fn x y))))

;; Note that no type checking is done to ensure that the new slot
;; values are of the necessary type.  However, some implementations
;; (like sbcl) check this on initialization (at least with safety set
;; to 3).
(interval-real-op trivial-interval add2  +)
(interval-real-op trivial-interval mult2 *)
(interval-real-op trivial-interval sub2  - t)
(interval-real-op trivial-interval div2  / t)

(defun add (&rest args)
  "Add up a couple of objects."
  (reduce #'add2 args))

(defun mult (&rest args)
  "Multiply arguments."
  (reduce #'mult2 args))

(defun sub (&rest numbers)
  "Substract"
  (reduce #'sub2 numbers))

(defun div (number &rest numbers)
  (div2 number (apply #'mult numbers)))



;;;; =========================================================================
;;;; Use Intervals as sets (Set Operations)
;;;; =========================================================================
(defconstant +empty-interval+ '+empty-interval+
  "The empty interval.  This is necessary to allow for set theoretic
  operations.")


(defclass abstract-multi-interval (abstract-interval)
  ()
  (:documentation "A multi interval is a sets of disjunct intervals.
  These may be constructed as a union of multiple intervals or by
  forming a complement of an interval."))

;;; Multi Interval (= a set of disjunct intervals)
(defclass multi-interval (abstract-interval)
  ((intervals :accessor intervals
              :initarg :intervals
              :type sequence))
  (:documentation "Most simple multi-interval class possible."))

(defun make-multi-interval (&rest intervals)
  (make-instance 'multi-interval
                 :intervals (sort intervals #'interval-lower-<)))

(defmethod interval-length ((mi abstract-multi-interval))
  (reduce #'+ (intervals mi) :key #'interval-length))


;;; ------------------------------------
;;; Generic Interval methods
(defun interval-empty-p (interval) 
  "Check if interval is empty."
  (eql interval +empty-interval+))

(defun singleton-p (interval)
  "Check if an interval consists only of single element."
  (= (lower-number interval) (upper-number interval)))

(defun interval-equal (i1 i2)
  (and (= (lower-number i1) (lower-number i2))
       (= (upper-number i1) (upper-number i2))))

(defun interval-= (i1 i2)
  (and (eql (type-of i1) (type-of i2))
       (interval-equal i1 i2)))


;;; ------------------------------------
;;; Helper functions

(declaim (inline interval-lower-< ordered2 ordered-overlap-p
                 ordered-adjacent ordered-union2 funcall-ordered))
(defun interval-lower-< (i1 i2)
  "Return true iff the lower bound of i1 is smaller than the lower
  number of i2.  Quite an important helper function.  In case non
  number types of boundaries have to be supported in some extension,
  this function is uppon the first that will have to be customized."
  (< (lower-number i1) (lower-number i2)))

(defun ordered2 (interval1 interval2)
  "Returns the two intervals as a cons, the one with the smaller lower
  bound first."
  (if (interval-lower-< interval1 interval2)
      (cons interval1 interval2)
      (cons interval2 interval1)))

(defgeneric ordered-overlap-p (i1 i2)
  (:documentation "Return true if i1 and i2 are adjacent or
  overlapping, and NIL otherwise.")
  (:method (i1 i2) (< (lower-number i2) (upper-number i1)))
  (:method ((i1 integer-interval) (i2 integer-interval))
    (<= (sub2 (lower-number i2) (upper-number i1)) 
        1)))

(defun overlap-p (i1 i2)
  "Return T iff the intervals overlap."
  (funcall-ordered #'ordered-overlap-p i1 i2))

(defun funcall-ordered (fn interval1 interval2)
  (destructuring-bind (low . high) (ordered2 interval1 interval2)
    (funcall fn low high)))

(defun list-of-simple-intervals (&rest intervals)
  (iter (for itv in intervals)
        (if (typep itv 'abstract-multi-interval)
            (nconcing (intervals itv))
            (collect itv))))


;;; ------------------------------------
;;; Union

(defun ordered-union2 (i1 i2)
  "Create the union of two overlapping intervals, which are given in
  order (smaller lower bound first)."
  (make-interval i1 (lower-number i1)
                 (max (upper-number i1) (upper-number i2))))

;; Ain't pretty but works
(defun ordered-union (&rest intervals)
  "Create the a list with the union of the intervals."
  (do* ((int-rest (cdr intervals) (cdr int-rest))
        (i1 (car intervals))
        (i2 (car int-rest) (car int-rest))
        (acc ()))
       ((null int-rest) 
        (nreverse (cond ((null (car acc)) (list i1))
                        ((ordered-overlap-p (car acc) i1)
                         (setf (car acc) (ordered-union2 (car acc) i1)))
                        (t (cons i1 acc)))))
    (if (ordered-overlap-p i1 i2)
        (setf i1 (ordered-union2 i1 i2))
        (progn (push i1 acc)
                (setf i1 i2)))))

(defun interval-union (&rest args)
  (let ((empty-removed   (remove-if #'interval-empty-p
                                    (apply #'list-of-simple-intervals args))))
    (if (null empty-removed)
        +empty-interval+
        (let ((union (apply #'ordered-union
                            (sort empty-removed #'interval-lower-<))))
          (if (null (cdr union))
              (car union)
              (make-instance 'multi-interval
                             :intervals union))))))


;;; -------------
;;; Intersections

;; Rewrite needed: It should be predictable which interval is cloned.

(defun ordered-intersection2 (i1 i2)
  (make-interval i1
                 (lower-number i2)
                 (min (upper-number i1) (upper-number i2))))

(defgeneric intersection2 (i1 i2)
  (:documentation "Intersection of two intervals")
  (:method (i1 i2)
    (funcall-ordered #'(lambda (i1 i2)
                         (if (ordered-overlap-p i1 i2)
                             (ordered-intersection2 i1 i2)
                             +empty-interval+))
                     i1 
                     i2))
  (:method ((mi1 abstract-multi-interval) (mi2 abstract-multi-interval))
    (apply #'interval-union
           (mapcar #'(lambda (x)
                       (intersection2 x mi2))
                   (intervals mi1)))))

(def-commutative-method intersection2 (si (ei (eql +empty-interval+)))
  (declare (ignore si))
  ei)

(def-commutative-method intersection2 (si (mi abstract-multi-interval))
  (apply #'interval-union
          (mapcar #'(lambda (x) (intersection2 si x))
                  (intervals mi))))


(defun interval-intersection (&rest intervals)
  (if (some #'interval-empty-p intervals)
      +empty-interval+
      (multiple-value-bind (lower upper)
          (iter (for itv in intervals)
                (maximizing (lower-number itv) into lo)
                (minimizing (upper-number itv) into up)
                (when (< up lo)
                  (return-from interval-intersection +empty-interval+))
                (finally (return (values lo up))))
        (make-interval (car intervals) lower upper))))


;;; ------------------------------------
;;; Misc Interval operations
(defgeneric interval-nth-element (interval n)
  (:documentation "Return the n-th element in the interval.")

  (:method ((interval integer-interval) (n integer))
    (+ (lower-number interval) (1- n)))

  (:method ((mi abstract-multi-interval) (n integer))
    (iter (for interval in (intervals mi))
          (for num-elements = (interval-length interval))
          (if (< num-elements n)
              (setf n (- n num-elements))
              (return (interval-nth-element interval n))))))



;;;; =========================================================================
;;;; #[x,y] Reader Macro
;;;; =========================================================================

(defparameter *terminal-\]*
  (copy-readtable))

(set-syntax-from-char #\] #\) *terminal-\]*)

;;; the #[ readmacro checks thoroughly
(set-dispatch-macro-character #\# #\[
  (lambda (stream sub-char infix-param)
    (declare (ignore sub-char infix-param))
    (let ((lower-number (read stream t nil t))
          (middle (read-char stream))
          (upper-number (let ((*readtable* *terminal-\]*))
                          (read stream t nil t)))
          (right-sq-bracket (read-char stream)))
      (check-type lower-number real)
      (check-type middle (eql #\,) "a comma.")
      (check-type upper-number real)
      (check-type right-sq-bracket (eql #\]))
      (make-instance (if (and (integerp lower-number)
                              (integerp upper-number))
                         'trivial-intervals:integer-interval
                         'trivial-intervals:trivial-interval)
                     :lower lower-number
                     :upper upper-number))))

;; ;; pretty-printing
;; (set-pprint-dispatch 'trivial-interval
;;   (lambda(stream object)
;;     (format stream "#[~A,~A]"
;;             (lower-number object)
;;             (upper-number object))))
(defmethod print-object ((object trivial-interval) (stream stream))
  (format stream "#[~A,~A]" (lower-number object) (upper-number object)))
