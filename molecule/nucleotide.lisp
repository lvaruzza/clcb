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

(in-package #:clcb)

(defclass nucleobase (molecule)
  ((1-letter-code :accessor 1-letter-code
                  :initarg :1-letter-code
                  :initform #\n
                  :type character)))


(defparameter *nucleobases*
  (mapcar #'(lambda (x)
              (apply #'make-instance 'nucleobase x))
          '((:name "Adenine"  :1-letter-code #\A)
            (:name "Guanine"  :1-letter-code #\G)
            (:name "Cytosine" :1-letter-code #\C)
            (:name "Thymine"  :1-letter-code #\T)
            (:name "Xanthine" :1-letter-code #\X)
            (:name "Uracil"   :1-letter-code #\U))))


(defgeneric get-nucleobase (identifier)
  (:documentation "Get an nucleobase object corresponding to the given
identifier.")
  (:method ((char character))
    (find char *nucleobases* :test #'char= :key #'1-letter-code)))


(defparameter *adenine* (get-nucleobase #\A))
(defparameter *thymine* (get-nucleobase #\T))
(defparameter *guanine* (get-nucleobase #\G))
(defparameter *cytosine* (get-nucleobase #\C))


(defgeneric num-hydrogen-bonds (mol1 mol2)
  (:documentation "Return the number of hydrogen bonds between 2 molecules.")
  (:method (mol1 mol2) (declare (ignore mol1 mol2)) 0))


(defmethod num-hydrogen-bonds ((nuc1 nucleobase) (nuc2 nucleobase))
  (flet ((bases-eq (base1 base2)
           (or (and (eq base1 nuc1) (eq base2 nuc2))
               (and (eq base2 nuc1) (eq base1 nuc2)))))
    (cond ((bases-eq *adenine* *thymine*) 2)
          ((bases-eq *guanine* *cytosine*) 3)
          (t 0))))

