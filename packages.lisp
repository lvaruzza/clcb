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

(defpackage clcb-utils
  (:documentation "Some simple utilities.")
  (:use #:cl
        #:cl-ppcre
        #:iterate
        #:split-sequence
        #:alexandria
	)
  (:nicknames #:utils #:clcb.utils)
  (:export ;; string utils
           #:string->type

           ;; misc utils
           #:shuffle-vector
           #:nshuffle-vector

           ;; file utils
           #:with-output-file
           #:with-input-file
           #:with-lines-from-file

           ;; tables
           #:read-table
           #:table
           #:table-row
           #:table-column
           #:table-num-rows
           #:column
           #:class->columns
           #:read-objects-from-table
           ;; new types in tables
           #:float-or-nil
           #:number-or-nil

           ;; interval
           #:next-element-fn
           #:previous-element-fn
           #:next-element-in-set
           #:previous-element-in-set
           #:element-member-fn
           #:+empty-interval+
           #:elementary-interval
           #:multi-interval
           #:integer-interval
           #:make-interval
           #:lower-bound
           #:upper-bound
           #:lower-bound-included-p
           #:upper-bound-included-p
           #:convex-hull
           #:interval-empty-p
           #:intervals
           #:interval-union
           #:interval-intersection
           #:interval-relative-complement
           #:interval-superset
           #:interval-elements
           #:interval-nth-element))



(defpackage clcb
  (:use #:cl
        #:cl-ppcre
        #:iterate
        #:alexandria
        #:clcb-config
        #:clcb-utils)
  (:export ;; molecule
           #:molecule
           #:molecule-name
           #:mol-weight

           ;; amino-acid
           #:amino-acid
           #:1-letter-code
           #:3-letter-code
           #:isoelectric-point
           #:pk1cooh
           #:pk2cooh
           #:pk1nh2
           #:pk2nh2
           #:hydropathy

           #:*amino-acids*
           #:*1-letter-aa-hash*
           #:*3-letter-aa-hash*
           #:get-amino-acid

	   ;; species
	   #:species
	   #:latin
	   #:trivial
	   #:ncbi
	   #:core-db-name
	   #:mart-db-name
	   #:short-name
	   #:*species*
	   #:*latin-species-hash*
	   #:*species-ncbi-id-hash*
	   #:stable-id->species
	   
           ;;; BIO-SEQUENCES
           #:nucleotide-sequence
           #:amino-acid-sequence
           #:alphabet
           #:feature

           ;; bio-sequence
           #:bio-sequence
           #:bio-sequence-id
           #:bio-sequence-name
           #:bio-sequence-description
           #:bio-sequence-seq
           #:bio-sequence-length
           #:nt-coords->aa-coords
           #:aa-coords->nt-coords

           ;; nucleotide-sequence
           #:circular
           #:alphabet

           #:shuffle-sequence


           ;;; SEQ-IO
           #:write-fasta
           #:parse-fasta-file
           #:read-next-fasta-entry

           ;; genetic-code
           #:codon
           #:codon-triplet
           #:codon-start-aa
           #:codon-aa
           #:genetic-code-table
           #:genetic-code-id
           #:genetic-code-name
           #:genetic-code-codons
           #:read-genetic-code-tables
           #:*genetic-code-tables*)
  (:documentation "The Common Lisp Computational Biology package
=============================================

This effort collects routines for sequence-based biological data."))
