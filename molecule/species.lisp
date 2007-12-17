;;;; Copyright (c) 2007 Albert Krewinkel, Steffen Moeller
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

(defclass species () 
  ((latin
     :accessor latin
     :type string
     :documentation "scientific name of species")
   (trivial
     :accessor trivial
     :initform nil :type string
     :documentation "common name of species")
   (ensembl_core
     :accessor core-db-name
     :initform nil :type string
     :documentation "name of core of SQL database")
   (ensembl_mart
     :accessor mart-db-name
     :initform nil :type string
     :documentation "name in EnsEMBL mart databases")
   (ensembl_gene
     :accessor short-name
     :initform nil :type string
     :documentation "abbreviation found in gene-, transcript- or protein-identifiers")
   (ncbi_id
     :accessor ncbi
     :initform nil :type utils::number-or-nil
     :documentation "ID in NCBI taxonomy database"))
  (:documentation "Utility class to prepare for comparisons of sequences between organisms.")



(defmethod print-object ((o species) (s stream))
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~A" (latin o))))


(defun read-species (species-table-file)
  "CLCB comes with a text file that describes the species and their appearance in EnsEMBL."
  (with-open-file (in aa-table-file)
    (read-objects-from-table in 'species)))

(defparameter *species*
  (read-species
   (make-pathname :name "species" :type "txt"
                  :defaults *data-directory-pathname*))
  "A global parameter with information about species in EnsEMBL. This file is manually created and may need an update when dealing with a newly sequenced organism that we were not yet aware of.

Example:

* (car *species*)

")

(defparameter *latin-species-hash* 
  (let ((latin-species-hash (make-hash-table)))
    (dolist (aa *species* latin-species-hash)
      (setf (gethash (latin aa) latin-species-hash)
            aa)
      (setf (gethash (char-downcase (latin aa)) latin-species-hash)
            aa)))
  "The objects representing species are retrievable only by their latin name.")
