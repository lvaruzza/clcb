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
  ((1-letter-code
    :accessor 1-letter-code     :initarg :1-letter-code 
    :initform #\X               :type    character
    :documentation "Single letter denoting an amino acid.")
   (3-letter-code
    :accessor 3-letter-code     :initarg :3-letter-code
    :initform "Xaa"             :type    string
    :documentation "Three letters denoting an amino acid.")
   (isoelectric-point
    :accessor isoelectric-point :initarg :isoelectric-point
    :initform 7f0               :type    utils::number-or-nil
    :documentation "The pH at which an amino acids becomes neutral.")
   (pk1COOH    :accessor pk1COOH    :initarg :pk1COOH    :initform nil
               :type     utils::number-or-nil)
   (pk2COOH    :accessor pk2COOH    :initarg :pk2COOH    :initform nil
               :type     utils::number-or-nil)
   (pk1NH2     :accessor pk1NH2     :initarg :pk1NH2     :initform nil
               :type     utils::number-or-nil)
   (pk2NH2     :accessor pk2NH2     :initarg :pk2NH2     :initform nil
               :type     utils::number-or-nil)
   (hydropathy :accessor hydropathy :initarg :hydropathy :initform nil
               :type     utils::number-or-nil
	       :documentation "The Kyte-Doolitle scoring of amino acids for their compatibility with polar water or apolar fatty acids. Albert."))
  (:documentation "The data directory of CLCB describes the naturally occuring amino acids with respect to their notation (1-letter and 3-letter codes) and biochemical properties. The individual lines are accessible as instances of this class."))

(defmethod print-object ((aa amino-acid) (s stream))
  "Print amino acid."
  (print-unreadable-object (aa s :type t :identity nil)
    (format s "~A" (3-letter-code aa))))


(defun read-amino-acids (aa-table-file)
  "Albert. "
  (with-open-file (in aa-table-file)
    (read-objects-from-table in 'amino-acid)))

(defparameter *amino-acids*
  "Albert. "
  (read-amino-acids
   (make-pathname :name "amino-acids" :type "txt"
                  :defaults *data-directory-pathname*)))

(defparameter *1-letter-aa-hash* 
  "The objects that are representing amino acids are stored in a hash from which they can be retrieved on demand, i.e, when reading and interpreting a sequence string."
  (let ((1-letter-hash (make-hash-table)))
    (dolist (aa *amino-acids* 1-letter-hash)
      (setf (gethash (1-letter-code aa) 1-letter-hash)
            aa)
      (setf (gethash (char-downcase (1-letter-code aa)) 1-letter-hash)
            aa))))

(defparameter *3-letter-aa-hash*
  "The objects representing amino acids are stored in a hash from which they can be retrieved via their 3-letter code, i.e., 'Ala will give your the object for Alanine. This function is used less frequently in everyday's practice, but, once one is working with artificial amino acids or those modifications performed by enzymes as post-translational modifications, to have more than the 26 letters of the alphabet becomes handy."
  (let ((3-letter-hash (make-hash-table :test #'equal)))
    (dolist (aa *amino-acids* 3-letter-hash)
      (setf (gethash (3-letter-code aa) 3-letter-hash) aa))))

;;;; -------------------------------------------------------------------------
;;;; Amino Acids Methods
;;;; -------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; Type definitions 
;;; FIXME: don't use, it would be too slow. (Defining those types was
;;; a nice exercise, though)
(let* ((1-letter-codes (mapcar #'1-letter-code *amino-acids*))
       (1-letter-codes-both-cases
        (nconc (mapcar #'char-upcase 1-letter-codes)
               (mapcar #'char-downcase 1-letter-codes)))
       (3-letter-codes (mapcar #'3-letter-code *amino-acids*)))
 (defun 1-letter-code-p (x)
   (find x 1-letter-codes-both-cases :test #'char=))

 (defun 3-letter-code-p (x)
   (when (typep x '(simple-array character *))
    (find x 3-letter-codes :test #'string=))))

(deftype 3-letter-code ()
  `(satisfies 3-letter-code-p))

(deftype 1-letter-code ()
  `(satisfies 1-letter-code-p))

;;; END: type declarations ---------------------------------------------------


(defgeneric get-amino-acid (identifier)
  (:documentation
   "Get an amino acid object corresponding to the given identifier.")

  (:method ((char character)) (gethash char *1-letter-aa-hash*))

  (:method ((3-letter-code string))
    (gethash (string-capitalize 3-letter-code) *3-letter-aa-hash*)))

