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

(in-package #:clcb-tests)

(deftestsuite bio-sequence-tests (clcb-tests)
  ((nt-seq (make-instance 'trivial-nucleotide-sequence
                           :id "nucleotide-test"
                           :seq "acttgaggaccatacat"))
   (aa-seq (make-instance 'trivial-amino-acid-sequence
                          :id "amino-acid-test"
                          :seq "TGHREQWMLYAGPIKDVH"))))

(deftestsuite class-hierarchies (bio-sequence-tests)
  ()
  (:tests
   (transcript
    (ensure (class-name (find-class 'clcb::transcript nil))))
   (trivial-transcript
    (ensure (class-name (find-class 'clcb::trivial-transcript nil))))
   (fragmented-transcript
    (ensure (class-name (find-class 'clcb::fragmented-transcript nil))))))

(addtest (class-hierarchies)
  trivial-nucleotide-superclass
  (ensure (subtypep 'trivial-transcript 'trivial-nucleotide-sequence)))

(addtest (class-hierarchies)
  fragmented-nucleotide-superclass
  (ensure (subtypep 'fragmented-transcript 'fragmented-nucleotide-sequence)))

;;; ------------------------------------
;;; Coordinate transformations
(deftestsuite coordinate-transformations (bio-sequence-tests)
  ()
  (:equality-test #'(lambda (x y)
                      (typecase x
                        (integer-interval (interval-= x y))
                        (integer (eql x y))))))

;; aa->nt
(addtest (coordinate-transformations)
  integer-aa->nt-1
  (ensure-same (aa-coords->nt-coords 1)
               #[1,3]
               :test #'interval-=))

(addtest (coordinate-transformations)
  integer-aa->nt-2
  (ensure-same (aa-coords->nt-coords 3)
               (make-interval 'integer-interval 7 9)
               :test #'interval-=))

(addtest (coordinate-transformations)
  interval-aa->nt-1
  (ensure-same (aa-coords->nt-coords #[1,3])
                #[1,9]
                :test #'interval-=))

(addtest (coordinate-transformations)
  interval-aa->nt-2
  (ensure-same (aa-coords->nt-coords #[3,10])
                #[7,30]
                :test #'interval-=))

;; nt->aa
(addtest (coordinate-transformations)
  integer-nt->aa-1
  (ensure-same (nt-coords->aa-coords 1)
               (values #[1,1] 0)))

(addtest (coordinate-transformations)
  integer-nt->aa-2
  (ensure-same (nt-coords->aa-coords #[8,30])
               (values #[3,10] 1 2)))

;; There and back again
(addtest (coordinate-transformations)
  aa->nt->aa
  (ensure-same (nt-coords->aa-coords (aa-coords->nt-coords #[3,15]))
               #[3,15]))


;;; ------------------------------------
;;; Subsequences
(deftestsuite bio-subseqs (bio-sequence-tests)
  ()
  (:equality-test #'(lambda (x y)
                      (and (string= (bio-sequence-seq x) (bio-sequence-seq y))
                           (interval-= x y)))))

(addtest (bio-subseqs)
  trivial-subseq-nt
  (ensure-same (bio-subseq nt-seq #[2,6])
               (make-instance 'trivial-nucleotide-sequence
                              :seq "cttga"
                              :seq-start 2
                              :seq-end   6)))
