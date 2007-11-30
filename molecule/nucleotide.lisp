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
                  :type character))
  (:documentation "Representation of bases in nucleotide sequences."))


(defparameter *nucleobases*
  (mapcar #'(lambda (x)
              (apply #'make-instance 'nucleobase x))
          '((:name "Adenine"     :1-letter-code #\A)
            (:name "Guanine"     :1-letter-code #\G)
            (:name "Cytosine"    :1-letter-code #\C)
            (:name "Thymine"     :1-letter-code #\T)
            (:name "Xanthine"    :1-letter-code #\X)
            (:name "Uracil"      :1-letter-code #\U)
            (:name "wildcard"    :1-letter-code #\N)
            (:name "A or G"      :1-letter-code #\R)
            (:name "C or T"      :1-letter-code #\Y)
            (:name "G or C"      :1-letter-code #\S)
            (:name "A or T"      :1-letter-code #\W)
            (:name "G or T"      :1-letter-code #\K)
            (:name "A or C"      :1-letter-code #\M)
            (:name "C or G or T" :1-letter-code #\S)
            (:name "A or G or T" :1-letter-code #\D)
            (:name "A or C or T" :1-letter-code #\H)
            (:name "A or C or G" :1-letter-code #\V)
	    ))
  "IUPAC single letter code for nucleotides (source:
http://www.bioinformatics.org/sms/iupac.html). Those entries that
represent an ambiguity should probably be modelled with a different
object. These are essential though for working with SNPs and hence
should be in.")


(defgeneric get-nucleobase (identifier)
  (:documentation "Get a nucleobase object corresponding to the given
identifier.")
  (:method ((char character))
    (find char *nucleobases* :test #'char= :key #'1-letter-code)))


(defparameter *adenine* (get-nucleobase #\A)
  "Global parameter storing the object for the nuleotide adine")
(defparameter *thymine* (get-nucleobase #\T)
  "Global parameter storing the object for the nuleotide thymine")
(defparameter *guanine* (get-nucleobase #\G)
  "Global parameter storing the object for the nuleotide guanine")
(defparameter *cytosine* (get-nucleobase #\C)
  "Global parameter storing the object for the nuleotide cytosine")


(defgeneric num-hydrogen-bonds (mol1 mol2)
  (:documentation "Return the number of hydrogen bonds between 2 molecules.")
  (:method (mol1 mol2) (declare (ignore mol1 mol2)) 0))


(defmethod num-hydrogen-bonds ((nuc1 nucleobase) (nuc2 nucleobase))
  "The Watson-Crick pairing of nuleotides on opposite strands is
supported by hydrogen bonds. A-T have 2 and G-C show 3. This
influences the annealing temperature of DNA and is said to influence
the accessibility of a genomic region to the polymerases. For RNA
structure prediction the here presented quick routine is not
sufficient since non-Watson-Crick pairings are also frequently
observed and a single base may have contacts with two others."
  (flet ((bases-eq (base1 base2)
           (or (and (eq base1 nuc1) (eq base2 nuc2))
               (and (eq base2 nuc1) (eq base1 nuc2)))))
    (cond ((bases-eq *adenine* *thymine*) 2)
          ((bases-eq *guanine* *cytosine*) 3)
          (t 0))))

