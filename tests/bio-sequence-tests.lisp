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
  ((nt-seq (make-instance 'nucleotide-sequence
                           :id "nucleotide-test"
                           :seq "acttgaggaccatacat"))
   (aa-seq (make-instance 'amino-acid-sequence
                          :id "amino-acid-test"
                          :seq "TGHREQWMLYAGPIKDVH"))))

(deftestsuite coordinate-transformations (bio-sequence-tests)
  ())


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
  interval-aa->nt
  (ensure-same (aa-coords->nt-coords #[1,3])
                #[1,9]
                :test #'interval-=))
