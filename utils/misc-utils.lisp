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

(in-package :clcb-utils)

(defun nshuffle-vector (vector)
  "Generate a random permutation of the vector using the Fisher-Yates
algorithm."
  ;; Adopted from code by Peter Seibel
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type vector vector))
  (do* ((idx (1- (length vector)) (1- idx))
        (other (random (1+ idx)) (random (1+ idx))))
       ((< idx 1) vector)
    (declare (type (integer 0 #.(1- array-total-size-limit)) idx))
    (unless (= idx other)
      (rotatef (aref vector idx)
               (aref vector other)))))

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))
