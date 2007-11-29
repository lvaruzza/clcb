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

(defun select-samples (n max)
  "Albert."
  (loop :for seen from 0
        :do (format t "~A " seen)
        :when (< (* (- max seen) (random 1.0)) n)
        :collect seen :and :do (decf n)
        :until (zerop n)))

;; FIXME: Slow and crap. rewrite
(defun unique-random-ints (n max)
  "Selection of n integers in the range of 0 to max."
  (when (> n max) (error "n > max, can't create that many unique integers"))
  (let ((result nil))
    (do ((i 0)
         (j (random max) (random max)))
        ((= n i) result)
      (unless (member j result :test #'eql)
          (progn (push j result)
                 (incf i))))))

(declaim (inline sqr))
(defun sqr (n)
  "Return the square of the number."
  (declare (type number n))
  (* n n))

(defun factorial (n)
  "Returns the factorial of the number n"
  (iter (for i from 2 to n)
        (multiply i)))
