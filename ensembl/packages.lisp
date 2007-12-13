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

(in-package :cl-user)
(defpackage clcb-ensembl
  (:use #:common-lisp
        #:clsql
        #:iterate
        #:clcb
        #:clcb-utils
        #:alexandria
	)
  (:nicknames #:ensembl)
  (:shadowing-import-from #:clcb-utils #:make-interval)
  (:export #:fetch-by-stable-id
           #:fetch-by-seq-region-name
           #:transcript
           #:gene
           #:exon
           #:translation
           #:codes-tm-protein
           #:codes-non-tm-protein
           #:protein-features
           #:protein-feature-type
           #:stable-id
           #:transcripts
           #:exons
           #:main-supporting-exon
           #:supporting-exons
           #:exon-phase
           #:strand

	   ;; data for sample code troughout CLCB
           #:sample-gene-tp53
	   #:sample-gene-rhodopsin

           ;; Stuff that will go into bio-sequence eventually
           #:dna-sequence-interval
           #:aa->nt
           #:nt->aa)
  (:documentation "Ensembl (accessible via http://www.ensembl.org) is a unique effort to organise the sequences of animal genomes and further related data on genes, proteins, sequence variation and intergenomic comparisons. The effort, it is all Open Source, has also been adopted for plants by other initiatives. Every species comes with a separate MySQL database that can be accessed via a common schema. CLCB took that (today accessibly invariant)  schema and pressed into Lisp classes."))

