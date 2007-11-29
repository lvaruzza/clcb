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
  "FASTA files may comprise many separate sequences. Each starts with a single line prefixed with a greate sign '>'. It first lists the accession number (or other sensible identifier) and optionally shows  some human readable additional information."
  (read-line stream))


(defun read-sequence-data (stream)
  "The sequence information is read from the stream. The header of the fasta file is expected to be removed before this function is called. Otherwise, an empty string is returned."
  (apply #'concatenate 'string
    (loop for line = (read-line stream nil nil)
          while (and line
                     (not (starts-with line ">")))
          collect line)))

(defun read-next-fasta-entry (stream)
  "Read and return the next fasta entry from the stream."
  (let ((name-line (read-line stream nil nil)))
    (if (not name-line)
        nil
        (let ((name (subseq name-line 1)))
          (make-instance 'bioseq
                         :id (first (split #\space name))
                         :name name
                         :seq (read-sequence-data stream))))))

(defun parse-fasta-file (file &key (single-entries-as-list nil))
  "Returns a list of bioseq objects, or a single bioseq object if the
file contains only one entry."
  (with-open-file (str file
                       :direction :input)
    (let ((res (loop for bioseq-obj = (read-next-fasta-entry str)
                     until (null bioseq-obj)
                     collect bioseq-obj)))
      (if (and (not single-entries-as-list)
               (null (cdr res)))
          (car res)
          res))))

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
