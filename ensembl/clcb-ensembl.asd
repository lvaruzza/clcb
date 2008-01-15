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

(defpackage clcb-ensembl-system
  (:use #:common-lisp
        #:asdf))

(in-package :clcb-ensembl-system)

(defsystem clcb-ensembl
    :description "EnsEMBL -- Access to the EnsEMBL genome database."
    :long-description "CLCB-ENSEMBL aims to provide a convenient
    interface to the EnsEMBL MySQL database via CLSQL and some
    additional wrapper classes."
    :version "0.1"
    :author "Albert Krewinkel <krewink@inb.uni-luebeck.de>"
    :maintainer "Albert Krewinkel <krewink@inb.uni-luebeck.de>"
    :licence "MIT"
    :depends-on ("iterate"
                 "clsql"
                 "clcb")
    ; :serial t
    :components
    ((:doc-file "README")
     (:file "packages")
     (:file "ensembl-classes"
            :depends-on ("packages"))
     (:file "ensembl-methods"
            :depends-on ("packages" "ensembl-classes"))
     (:file "compara-methods"
     	    :depends-on ("packages" "ensembl-classes" "ensembl-methods"))))
