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
  ((latin-name
    :accessor species-latin-name
    :initarg :latin-name
    :type string
    :documentation "Scientific name of species.")
   (trivial-name
    :accessor species-trivial-name
    :initarg :trivial-name
    :initform nil :type string
    :documentation "Common name of species.")
   (ensembl-core-db
    :accessor species-ensembl-core-db
    :initarg :ensembl-core-db
    :initform nil :type string
    :documentation "Name of core of SQL database.")
   (ensembl-mart-db
    :accessor species-ensembl-mart-db
    :initarg :ensembl-mart-db
    :initform nil :type string
    :documentation "Name in EnsEMBL mart databases.")
   (ensembl-gene
    :accessor species-ensembl-gene
    :initarg :ensembl-gene
    :initform nil :type string
    :documentation "Abbreviation found in gene-, transcript- or
     protein-identifiers.")
   (ncbi-id
     :accessor species-ncbi-id
     :initarg :ncbi-id
     :initform nil :type clcb-utils::number-or-nil
     :documentation "ID in NCBI taxonomy database"))
  (:documentation "Utility class to prepare for comparisons of
  sequences between organisms."))



(defmethod print-object ((o species) (s stream))
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~A" (species-latin-name o))))


(defun read-species (species-table-file)
  "CLCB comes with a text file that describes the species and their
appearance in EnsEMBL."
  (with-open-file (in species-table-file)
    (read-objects-from-table in 'species)))


(defparameter *species*
  (read-species
   (make-pathname :name "species" :type "txt"
                  :defaults *data-directory-pathname*))
  "A global parameter with information about species.  Parameters are
read from the file \"data/species.txt\".  This file is manually
created and may need an update when dealing with a newly sequenced
organism that we were not yet aware of.") 


(defparameter *species-latin-hash* 
  (let ((species-latin-hash (make-hash-table :test 'equal)))
    (dolist (species *species* species-latin-hash)
      (setf (gethash (species-latin-name species) species-latin-hash)
            species)))
  "The objects representing species are retrievable only by their latin name.")
