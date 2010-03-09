;;;; Copyright (c) 2010 Leonardo Varuzza
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


(defun read-fastq-header (stream)
  "FASTA files may comprise many separate sequences. Each starts with
   a single line prefixed with a greate sign '>'.  It first lists the
   accession number (or other sensible identifier) and optionally
   shows some human readable additional information."
  (loop for line = (read-line stream)
     when (starts-with #\@ line)
     return (subseq line 1)))


(defun read-line-until-char (char stream)
  (apply #'concatenate 'string
    (loop
       for line = (read-line stream nil nil)
       while (and line (< 0 (length line)))
       collect line
       until (char= (peek-char t stream nil char) char))))
  
(defun read-fastq-sequence-data (stream)
  "The sequence information is read from the stream.  The header of the
  fasta file is expected to be removed before this function is
  called.  Otherwise, an empty string is returned."
  (let ((seq (read-line-until-char #\+ stream))
	(second-header (read-line stream))
	(qual (read-line-until-char #\@ stream)))
    (declare (ignore second-header))
    (values seq qual)))

(defun read-next-fastq-entry (&optional (stream *standard-input*)
                              (eof-error-p t)
                              eof-value)
  "Read and return the next fasta entry from the stream."
  (handler-case
      (let ((name-line (read-fastq-header stream)))
	(multiple-value-bind (seq qual) 
	    (read-fastq-sequence-data stream)
	  (values 
	   (make-instance 'bio-sequence-record
			  :id (first (split #\space name-line))
			  :name name-line
			  :seq (delete #\space seq))
	   (make-instance 'bio-sequence-record
			  :id (first (split #\space name-line))
			  :name name-line
			  :seq (delete #\space qual)))))


     (end-of-file (c)
       (if eof-error-p
           (error c)
           eof-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter  +test-fastq+ 
"@SRR033650.56 solid0045_20080529_1_C_elegans_1_21_111 length=25
T1230020300000133100121222
+SRR033650.56 solid0045_20080529_1_C_elegans_1_21_111 length=25
!&/&-6&%(&$#&%$'+$'2$#&($,
@SRR033650.57 solid0045_20080529_1_C_elegans_1_21_121 length=25
T3033123312113323202133031
+SRR033650.57 solid0045_20080529_1_C_elegans_1_21_121 length=25
!#712&%=233',;>-31(&(&3%,)
@SRR033650.58 solid0045_20080529_1_C_elegans_1_21_149 length=25
T3102312033011321132223021
+SRR033650.58 solid0045_20080529_1_C_elegans_1_21_149 length=25
!##)#######*##############
@SRR033650.59 solid0045_20080529_1_C_elegans_1_21_189 length=25
T0311301121303120000001332
+SRR033650.59 solid0045_20080529_1_C_elegans_1_21_189 length=25
!@63;:@@:<6<?77=<@.?@9:&6;
@SRR033650.60 solid0045_20080529_1_C_elegans_1_21_260 length=25
T0203230303020211001200101
+SRR033650.60 solid0045_20080529_1_C_elegans_1_21_260 length=25
!=6>@79;?<B=>=87,75@55<&<5
@SRR033650.61 solid0045_20080529_1_C_elegans_1_21_605 length=25
T2130201331102223111102330
+SRR033650.61 solid0045_20080529_1_C_elegans_1_21_605 length=25
!99.9?<?97<3<+9=506=:48$77
@SRR033650.62 solid0045_20080529_1_C_elegans_1_21_618 length=25
T0021311102221031013113023
+SRR033650.62 solid0045_20080529_1_C_elegans_1_21_618 length=25
!7A6175?7:;4578<1>)21,6))4
@SRR033650.63 solid0045_20080529_1_C_elegans_1_21_647 length=25
T3013002022023011323103332
+SRR033650.63 solid0045_20080529_1_C_elegans_1_21_647 length=25
!23'//6:'.174(=9*0))2-9#/&
@SRR033650.64 solid0045_20080529_1_C_elegans_1_21_652 length=25
T1200120201011031322032210")
  
			    
(defun test-fastq () 
  (multiple-value-bind
	(seq qual) 
      (read-next-fastq-entry (make-string-input-stream +test-fastq+))
    (print (list seq qual))
    (print (list (seq seq)
		 (seq qual)))))

