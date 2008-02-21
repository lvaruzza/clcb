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

(defpackage clcb-system
  (:use #:common-lisp
        #:asdf))

(in-package :clcb-system)

(defsystem clcb
    :description "CLCB - Common Lisp stuff for computational biology."
    :long-description
    "CLCB is a small project initiated by Albert Krewinkel, mostly to teach
     himself some lisp. Over time, CLCB has turned into a tool with some
     competitive edge for research in computational biology."
    :version "0.1"
    :author "Albert Krewinkel <krewink@inb.uni-luebeck.de>"
    :maintainer "Albert Krewinkel <krewink@inb.uni-luebeck.de>"
    :licence "MIT"
    :depends-on ("iterate"
                 "cl-ppcre"
                 "split-sequence";; Why doesn't ppcre suffice?
                 "moptilities"
                 "alexandria"
		 "metabang-bind"
                 "trivial-intervals"
		 )
    :components
    ((:doc-file "README")
     (:static-file "LICENSE")
     
     (:file "config")
     (:file "packages" :depends-on ("config"))
     (:file "genetic-code" :depends-on ("packages" "config" "molecule"))
     
     (:module "utils"
              :components ((:file "math-utils")
                           (:file "misc-utils")
                           (:file "tables")
                           (:file "file-utils")
                           (:file "string-utils")
                           #+nil(:file "interval"))
              :depends-on ("packages"))

     (:module "data"
              :components ((:static-file "amino-acids.txt")
                           (:static-file "genetic-code.prt")))

     (:module "molecule"
              :components ((:file "molecule")
                           (:file "amino-acids")
                           (:file "species")
                           (:file "nucleotide"))
              :depends-on ("utils" "config")
              :serial t)

     (:module "bio-sequence"
              :components ((:file "bio-sequence"))
              :depends-on ("packages" "utils" "molecule"))
     (:module "seq-io"
              :components ((:file "fasta"))
              :depends-on ("packages"))))
