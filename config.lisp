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

(defpackage #:clcb-config
  (:use #:cl)
  (:export #:*data-directory-pathname*)
  (:documentation "The config package allows for adapting the CLCB installation to the local setup."))

(in-package #:clcb-config)

(defvar *data-directory-pathname*
  (merge-pathnames (make-pathname :directory '(:relative "data"))
                   (asdf:component-pathname (asdf:find-system :clcb))))

(defvar *ensembl-host*     "pc13.inb.uni-luebeck.de")
(defvar *ensembl-database* "homo_sapiens_core_47_36i")
(defvar *ensembl-user*     "qtl")
(defvar *ensembl-password*   "")
(defvar *ensembl-db-type*  :mysql)

(defun ensembl-connection-data ()
  `(,(list *ensembl-host* *ensembl-database* *ensembl-user* *ensembl-password*)
    ,@(when *ensembl-db-type* (list :database-type *ensembl-db-type*))))
