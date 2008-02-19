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

(defun read-fasta-header (stream)
  "FASTA files may comprise many separate sequences. Each starts with
   a single line prefixed with a greate sign '>'.  It first lists the
   accession number (or other sensible identifier) and optionally
   shows some human readable additional information."
  (loop for line = (read-line stream)
     when (starts-with #\> line)
     return (subseq line 1)))


(defun read-sequence-data (stream)
  "The sequence information is read from the stream.  The header of the
  fasta file is expected to be removed before this function is
  called.  Otherwise, an empty string is returned."
  (apply #'concatenate 'string
    (loop
       for line = (read-line stream nil nil)
       while (and line (< 0 (length line)))
       collect line
       until (char= (peek-char t stream nil #\>) #\>))))

(defun read-next-fasta-entry (&optional (stream *standard-input*)
                              (eof-error-p t)
                              eof-value)
  "Read and return the next fasta entry from the stream."
  (handler-case
      (let ((name-line (read-fasta-header stream)))
        (make-instance 'bio-sequence-record
                       :id (first (split #\space name-line))
                       :name name-line
                       :seq (delete #\space (read-sequence-data stream))))
     (end-of-file (c)
       (if eof-error-p
           (error c)
           eof-value))))

(defun parse-fasta-file (file &key (single-entry-as-list nil))
  "Returns a list of bioseq objects, or a single bioseq object if the
   file contains only one entry."
  (with-open-file (str file :direction :input)
    (let ((res (loop for bioseq-obj = (read-next-fasta-entry str)
                     until (null bioseq-obj)
                     collect bioseq-obj)))
      (if (and (not single-entry-as-list)
               (null (cdr res)))
          (car res)
          res))))


;;;; =========================================================================
;;;; Iterate generator for easy handling of the entries
;;;; =========================================================================
(defmacro do-fasta-records ((var stream &optional result) &body body)
  (let ((block-name (gensym "DO-FASTA-BLOCK")))
   `(block ,block-name
      (loop for ,var = (read-next-fasta-entry ,stream nil nil)
            until (null ,var)
            do (progn ,@body))
      (return-from ,block-name ,result))))
 


;;;; =========================================================================
;;;; Write Fasta File
;;;; =========================================================================
(defun write-fasta (bioseq stream)
  "Write bioseq to stream in fasta format."
  (format stream ">~A~%" (bio-sequence-name bioseq))
  (loop for i upfrom 0 by 60
        while (< i (bio-sequence-length bioseq))
        do (format stream "~A~%"
                   (subseq (bio-sequence-seq bioseq) 
                           i
                           (min (bio-sequence-length bioseq) (+ i 60))))))

#+test
(defun foo (&optional 
            (file #p"/nfshome/krewink/bio/spielzeug/hsapiens-5ht2c.fasta"))
  (with-open-file (str file)
    (read-next-fasta-entry str)))
