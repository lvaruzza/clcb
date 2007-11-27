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

(in-package :clcb)

(defclass codon ()
  ((triplet :initarg :triplet
	    :documentation "describes three nucleotides that need to be matched on the mRNA"
            :accessor codon-triplet)
   (start-aa :initarg :start-aa
             :documentation "set if the codon may initiate a translation."
             :accessor codon-start-aa)
   (aa :initarg :aa
       :documentation "the amino acid that is transferred"
       :accessor codon-aa))
   (:documentation "Codons can be though of units of translation from RNA to protein. Every codon is assigned to a particular tRNA, which may recognise multiple codons. There are two types of special codons, one that may initiate the translation process (see slot start-aa), the other that certainly stops it. In some organism under some special conditions such stop codons may also code for the 21st amino acid, seleno-cystein, but this is not modelled here."))

(defmethod print-object ((codon codon) (s stream))
  (with-accessors ((aa codon-aa) (saa codon-start-aa)) codon
   (print-unreadable-object (codon s :type t :identity nil)
       (format s "~A->~A~@[ START->~A~]"
               (codon-triplet codon)
               (if aa (1-letter-code aa) "*")
               (if saa (1-letter-code saa) nil)))))

(defclass genetic-code-table ()
  ((id :accessor genetic-code-id
       :documentation "A number to which is commonly referred when addressing a particular coding table."
       :initarg :id)
   (name :accessor genetic-code-name
	 :documentation "The human-understandable description of the range of species that features this translation"
         :initarg :name)
   (codons :initarg :codons
	   :documentation "List of objects of the codon class that assign nucleotides to amino acids."
           :accessor genetic-code-codons))
   (:documentation "Species possibly differ in their assignments of nucleotides to amino acids. Actually, our mitochondria have a different (bacterial) coding from our nucleus. The assignment is referred to as the genetic code and this class knows about multiple such codes that it represents in a table."))

(defmethod print-object ((gct genetic-code-table) (s stream))
  (print-unreadable-object (gct s :type t)
    (format s "'~A'" (genetic-code-name gct))))

(defun create-codon-list (ncbieaa sncbieaa)
  (flet ((nth-table-codon-string (i)
           (let ((nucleotides #(#\t #\c #\a #\g)))
             (map 'string
                  #'(lambda (n) (aref nucleotides (mod (truncate i n) 4)))
                  #(16 4 1)))))
    (loop for i upfrom 0
          for aa across ncbieaa
          for saa across sncbieaa
          collect (make-instance 'codon
                                 :triplet (nth-table-codon-string i)
                                 :start-aa (if (char/= #\- saa)
                                               (get-amino-acid saa)
                                               nil)
                                 :aa (if (char/= #\* aa)
                                         (get-amino-acid aa)
                                         nil)))))



(defun read-next-genetic-code-table (stream)
  (flet ((first-match (regex string)
           (multiple-value-bind (full-hit marked-hits)
               (ppcre:scan-to-strings regex string)
             (declare (ignore full-hit))
             (aref marked-hits 0))))
    (let (name
          id
          ncbieaa
          sncbieaa)
      (loop for line = (read-line stream nil nil)
            for i below 1000
            while (and (not (null line))
                       (not (ppcre:scan "^\\s*}\\s*,\\s*$" line)))
            do (cond ((ppcre:scan "^\\s+name" line)
                      (unless name
                        (setf name 
                              (first-match "name\\s+\"(.+)\"" line))))
                     ((ppcre:scan "^\\s+id" line)
                      (setf id
                            (parse-integer
                             (first-match "id\\s+(\\w+)" line))))
                     ((ppcre:scan "^\\s+sncbieaa" line)
                      (setf sncbieaa
                            (first-match "sncbieaa\\s+\"(.+\)\"" line)))
                     ((ppcre:scan "^\\s+ncbieaa" line)
                      (setf ncbieaa
                            (first-match "ncbieaa\\s+\"(.+)\"" line)))))
      (if (not (and name sncbieaa ncbieaa id))
          nil
          (make-instance 'genetic-code-table
                         :name name
                         :id id
                         :codons (create-codon-list ncbieaa sncbieaa))))))

(defun read-genetic-code-tables
    (&optional (filename (make-pathname :name "genetic-code"
                                        :type "prt"
                                        :defaults *data-directory-pathname*)))
  (with-open-file (in filename)
    (loop for table = (read-next-genetic-code-table in)
          while table
          collect table)))

(defparameter *genetic-code-tables* (read-genetic-code-tables))
(defparameter *default-genetic-code* (first *genetic-code-tables*))

(defgeneric get-codon (identifier)
  (:documentation "Get the codon defined by `identifier' with
  respect to the *default-genetic-code* variable.")

  (:method ((codon codon)) codon)

  (:method ((str string))
    (find (string-downcase str) (genetic-code-codons *default-genetic-code*)
          :key #'codon-triplet :test #'string=)))

(defmacro with-codons-from-string ((codon string) &body body)
  (let ((i (gensym "CODON-COUNTER"))
        (s (gensym "SEQ")))
    `(let ((,s ,string))
      (loop for ,i from 0 below (length ,s) by 3
       for ,codon = (get-codon (subseq ,s ,i (+ ,i 3)))
       do (progn ,@body)))))


(defmethod 1-letter-code ((aa (eql nil)))
  #\*)

(defgeneric translate (sequence &key to-string)
  (:documentation "Translate a DNA or RNA sequence to protein."))

(defmethod translate ((str string) &key (to-string nil))
  (funcall (if to-string
               #'(lambda (x) (map 'string #'1-letter-code x))
               #'identity)
           (let ((trans '()))
             (with-codons-from-string (cod str)
               (push (codon-aa cod) trans))
             (nreverse trans))))


(defun random-dna-sequence (length)
  (let ((seq (make-array length :element-type 'character)))
    (loop for i below length
          for j = (random 4)
          with bases = "ACGT"
          do (setf (aref seq i) (aref bases j)))
    seq))

;; util function
(defun remove-nils (list)
  (remove-if #'null list))

(defmethod get-amino-acid ((cod codon))
  (codon-aa cod))

(defun string->amino-acids (string)
  "Create a vector of amino acids by interpreting the string as a
sequence of 1-letter-codes."
  (declare (optimize (speed 3) (safety 1))
           (type simple-string string))
  (map '(simple-array amino-acid (*)) #'get-amino-acid string))
