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

(defclass amino-acid (molecule)
  ((name
    :type string)
   (mol-weight
    :initform nil                :type clcb-utils::number-or-nil)
   (1-letter-code
    :accessor 1-letter-code     :initarg :1-letter-code 
    :initform #\X               :type    character
    :documentation "Single letter denoting an amino acid.")
   (3-letter-code
    :accessor 3-letter-code     :initarg :3-letter-code
    :initform "Xaa"             :type    string
    :documentation "Three letters denoting an amino acid.")
   (isoelectric-point
    :accessor isoelectric-point :initarg :isoelectric-point
    :initform 7f0               :type    clcb-utils::number-or-nil
    :documentation "The pH at which an amino acids stops moving in an
    electic field.")
   (pk1COOH    :accessor pk1COOH    :initarg :pk1COOH    :initform nil
               :type     clcb-utils::number-or-nil)
   (pk2COOH    :accessor pk2COOH    :initarg :pk2COOH    :initform nil
               :type     clcb-utils::number-or-nil)
   (pk1NH2     :accessor pk1NH2     :initarg :pk1NH2     :initform nil
               :type     clcb-utils::number-or-nil)
   (pk2NH2     :accessor pk2NH2     :initarg :pk2NH2     :initform nil
               :type     clcb-utils::number-or-nil)
   (hydropathy :accessor hydropathy :initarg :hydropathy :initform nil
               :type     clcb-utils::number-or-nil
	       :documentation "The Kyte-Doolitle scoring of amino
	       acids for their compatibility with polar water or
	       apolar fatty acids."))
  (:documentation "The data directory of CLCB describes the naturally
  occuring amino acids with respect to their notation (1-letter and
  3-letter codes) and biochemical properties. The individual lines are
  accessible as instances of this class."))

(defmethod print-object ((aa amino-acid) (s stream))
  (print-unreadable-object (aa s :type t :identity nil)
    (format s "~A" (3-letter-code aa))))

(defmethod make-load-form ((aa amino-acid)  &optional environment)
  (make-load-form-saving-slots 
   aa
   :environment environment
   :slot-names (mapcar #'slot-definition-name
                       (class-slots (class-of aa)))))


(defun read-amino-acids (aa-table-file)
  "CLCB comes with a text file that describes the most essential
properties of amino acids. You may have your own. This function
creates instances for the amino acids stored in that file."
  (with-open-file (in aa-table-file)
    (read-objects-from-table in 'amino-acid)))

(defparameter *amino-acids*
  ;; My table reader code is too buggy, therefor the objects are initialized
  ;; explicitly.
  (list
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Alanine" :3-LETTER-CODE "Ala"
                  :1-LETTER-CODE #\A :PK1COOH 2.3 :PK1NH2 9.9
                  :ISOELECTRIC-POINT 6.1 :HYDROPATHY 1.8) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Arginine" :3-LETTER-CODE "Arg"
                  :1-LETTER-CODE #\R :PK1COOH 2.81 :PK1NH2 9.09 :PK2NH2 13.2
                  :ISOELECTRIC-POINT 11.76 :HYDROPATHY -4.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Asparagine" :3-LETTER-CODE "Asn"
                  :1-LETTER-CODE #\N :PK1COOH 2.02 :PK1NH2 8.8
                  :ISOELECTRIC-POINT 5.41 :HYDROPATHY -3.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Aspartic acid" :3-LETTER-CODE "Asp"
                  :1-LETTER-CODE #\D :PK2COOH 1.88 :PK1COOH 3.65 :PK1NH2 9.6
                  :ISOELECTRIC-POINT 2.85 :HYDROPATHY -3.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Cysteine" :3-LETTER-CODE "Cys"
                  :1-LETTER-CODE #\C :PK2COOH 1.71 :PK1COOH 8.33 :PK1NH2
                  10.78 :ISOELECTRIC-POINT 5.05 :HYDROPATHY 2.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Glutamic acid" :3-LETTER-CODE "Glu"
                  :1-LETTER-CODE #\E :PK2COOH 2.19 :PK1COOH 4.25 :PK1NH2 9.67
                  :ISOELECTRIC-POINT 3.22 :HYDROPATHY -3.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Glutamine" :3-LETTER-CODE "Gln"
                  :1-LETTER-CODE #\Q :PK1COOH 2.17 :PK1NH2 9.13
                  :ISOELECTRIC-POINT 5.65 :HYDROPATHY -3.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Glycine" :3-LETTER-CODE "Gly"
                  :1-LETTER-CODE #\G :PK1COOH 2.21 :PK1NH2 9.15
                  :ISOELECTRIC-POINT 5.97 :HYDROPATHY -0.4) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Histidine" :3-LETTER-CODE "His"
                  :1-LETTER-CODE #\H :PK1COOH 1.78 :PK1NH2 8.97 :PK2NH2 5.97
                  :ISOELECTRIC-POINT 7.47 :HYDROPATHY -3.2) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Isoleucine" :3-LETTER-CODE "Ile"
                  :1-LETTER-CODE #\I :PK1COOH 2.32 :PK1NH2 9.76
                  :ISOELECTRIC-POINT 5.94 :HYDROPATHY 4.5) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Leucine" :3-LETTER-CODE "Leu"
                  :1-LETTER-CODE #\L :PK1COOH 2.4 :PK1NH2 9.6
                  :ISOELECTRIC-POINT 5.98 :HYDROPATHY 3.8) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Lysine" :3-LETTER-CODE "Lys"
                  :1-LETTER-CODE #\K :PK1COOH 2.2 :PK1NH2 8.9 :PK2NH2 10.28
                  :ISOELECTRIC-POINT 9.59 :HYDROPATHY -3.9) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Methionine" :3-LETTER-CODE "Met"
                  :1-LETTER-CODE #\M :PK1COOH 2.28 :PK1NH2 9.21
                  :ISOELECTRIC-POINT 5.74 :HYDROPATHY 1.9) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Phenylalanine" :3-LETTER-CODE "Phe"
                  :1-LETTER-CODE #\F :PK1COOH 2.58 :PK1NH2 9.24
                  :ISOELECTRIC-POINT 5.84 :HYDROPATHY 2.8) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Proline" :3-LETTER-CODE "Pro"
                  :1-LETTER-CODE #\P :PK1COOH 1.99 :PK1NH2 10.6
                  :ISOELECTRIC-POINT 6.3 :HYDROPATHY -1.6) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Serine" :3-LETTER-CODE "Ser"
                  :1-LETTER-CODE #\S :PK1COOH 2.21 :PK1NH2 9.15
                  :ISOELECTRIC-POINT 5.68 :HYDROPATHY -0.8) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Threonine" :3-LETTER-CODE "Thr"
                  :1-LETTER-CODE #\T :PK1COOH 2.1 :PK1NH2 9.12
                  :ISOELECTRIC-POINT 5.6 :HYDROPATHY -0.7) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Tryptophan" :3-LETTER-CODE "Trp"
                  :1-LETTER-CODE #\W :PK1COOH 2.15 :PK1NH2 9.12
                  :ISOELECTRIC-POINT 5.64 :HYDROPATHY -0.9) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Tyrosine" :3-LETTER-CODE "Tyr"
                  :1-LETTER-CODE #\Y :PK2COOH 2.2 :PK1COOH 9.11 :PK1NH2 10.07
                  :ISOELECTRIC-POINT 5.66 :HYDROPATHY -1.3) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Valine" :3-LETTER-CODE "Val"
                  :1-LETTER-CODE #\V :PK1COOH 2.3 :PK1NH2 9.6
                  :ISOELECTRIC-POINT 5.96 :HYDROPATHY 4.2) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Selenocysteine" :3-LETTER-CODE "Sec"
                  :1-LETTER-CODE #\U :PK1COOH 5.3) 
   (MAKE-INSTANCE 'AMINO-ACID :NAME "Pyrrolysine" :3-LETTER-CODE "Pyl"
                  :1-LETTER-CODE #\O))
  #+nil(read-amino-acids
   (make-pathname :name "amino-acids" :type "txt"
                  :defaults *data-directory-pathname*))
  "A global parameter with information about all naturally occurring
  amino acids.")

(defparameter *1-letter-aa-hash* 
  (let ((1-letter-hash (make-hash-table)))
    (dolist (aa *amino-acids* 1-letter-hash)
      (setf (gethash (1-letter-code aa) 1-letter-hash)
            aa)
      (setf (gethash (char-downcase (1-letter-code aa)) 1-letter-hash)
            aa)))
  "The objects that are representing amino acids are stored in a hash
from which they can be retrieved on demand, i.e, when reading and
interpreting a sequence string.

Example:

* *1-letter-aa-hash*

#<HASH-TABLE :TEST EQL :COUNT 44 {1002AA0811}>

* (gethash #\a *1-letter-aa-hash*)

#<AMINO-ACID Ala>
T")

(defparameter *3-letter-aa-hash*
  (let ((3-letter-hash (make-hash-table :test #'equal)))
    (dolist (aa *amino-acids* 3-letter-hash)
      (setf (gethash (3-letter-code aa) 3-letter-hash) aa)))
  "The objects representing amino acids are stored in a hash from
which they can be retrieved via their 3-letter code, i.e., 'Ala will
give your the object for Alanine. This function is used less
frequently in everyday's practice, but, once one is working with
artificial amino acids or those modifications performed by enzymes as
post-translational modifications, to have more than the 26 letters of
the alphabet becomes handy.

*3-letter-aa-hash*

#<HASH-TABLE :TEST EQUAL :COUNT 22 {1002FF6F81}> ")

;;;; -------------------------------------------------------------------------
;;;; Amino Acids Methods
;;;; -------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; Type definitions 
;;; FIXME: don't use, it would be too slow. (Defining those types was
;;; a nice exercise, though)
;; (let* ((1-letter-codes (mapcar #'1-letter-code *amino-acids*))
;;        (1-letter-codes-both-cases
;;         (nconc (mapcar #'char-upcase 1-letter-codes)
;;                (mapcar #'char-downcase 1-letter-codes)))
;;        (3-letter-codes (mapcar #'3-letter-code *amino-acids*)))
;;  (defun 1-letter-code-p (x)
;;    (find x 1-letter-codes-both-cases :test #'char=))

;;  (defun 3-letter-code-p (x)
;;    (when (typep x '(simple-array character *))
;;     (find x 3-letter-codes :test #'string=))))

;; (deftype 3-letter-code ()
;;   `(satisfies 3-letter-code-p))

;; (deftype 1-letter-code ()
;;   `(satisfies 1-letter-code-p))

;;; END: type declarations ---------------------------------------------------


(defgeneric get-amino-acid (identifier)
  (:documentation "Get an amino acid object corresponding to the given
  identifier.
       
   ### Example:
   * (get-amino-acid \"ala\")
   #<AMINO-ACID Ala>
   T")

  (:method ((char character)) (gethash char *1-letter-aa-hash*))

  (:method ((3-letter-code string))
    (gethash (string-capitalize 3-letter-code) *3-letter-aa-hash*)))

