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

(in-package :ensembl)


;(connect '("ensembldb.ensembl.org" "homo_sapiens_core_47_36i" "anonymous" "")
;:database-type :mysql)
(apply #'connect (clcb-config::ensembl-connection-data))


;;; EnsEMBL superclass
(def-view-class ensembl-object () ())

;;; EnsEMBL error conditions
(define-condition ensembl-error () ())

(define-condition ensembl-fetch-error (ensembl-error)
  ((object-to-fetch :initarg :object-to-fetch :reader object-to-fetch)))



;;; =========================================================================
;;; Macro Definitions -- ease the process of defining EnsEMBL views.
;;; =========================================================================
(defgeneric fetch-by-stable-id-and-type (stable-id object-type)
  (:documentation
   "Get an EnsEMBL object of type `object-type' by its stable id."))


(defmacro with-symbol-prefixing-function ((fn-name prefix) &body body)
  `(flet ((,fn-name (&optional (suffix "")) 
            (format-symbol t "~A~A" ,prefix suffix)))
     ,@body))

(defparameter *char-class-alist* ()
  "An alist that maps from characters to ensembl-classes. It's
intended to be used in conjuction with stable ids.")

;;; Method writer
;;; -------------
(defmacro def-fetch-by-stable-id-method (ensembl-class)
  "Define a method to access an object by its stable id."
  (with-symbol-prefixing-function (table-concat ensembl-class)
    (let ((stable-id-view (table-concat '-stable-id))
          (object-type (table-concat))
          (db-id (table-concat '-id)))
      `(progn
        (defmethod fetch-by-stable-id-and-type
            ((stable-id string) (obj-type (eql ',object-type)))
          (let ((fetched-object
                 (car
                  (select ',object-type :where
                          (sql-and
                           (sql-= (sql-slot-value ',stable-id-view ',db-id)
                                  (sql-slot-value ',object-type ',db-id))
                           (sql-= (sql-slot-value ',stable-id-view 'stable-id)
                                  stable-id))
                          :flatp t))))
            (if fetched-object
                fetched-object
                (error 'ensembl-fetch-error :object-to-fetch stable-id))))
        (defmethod fetch-by-stable-id-and-type
            ((stable-ids list) (obj-type (eql ',object-type)))
          (let ((fetched-objects
                 (select ',object-type :where
                         (sql-and
                          (sql-= (sql-slot-value ',stable-id-view ',db-id)
                                 (sql-slot-value ',object-type ',db-id))
                          (sql-in (sql-slot-value ',stable-id-view 'stable-id)
                                  stable-ids))
                         :flatp t)))
            fetched-objects))))))


;;; Ensembl Object definitions
;;; --------------------------
(defmacro def-ensembl-view (name superclasses slots &rest options)
  "Define an EnsEMBL class."
  (let ((supers (remove-duplicates (append superclasses '(ensembl-object))))
        (cl-options
         (nconc (list :base-table (substitute #\_ #\- (string-downcase name)))
                options)))
    `(def-view-class ,name ,supers ,slots ,cl-options)))

(defmacro def-ensembl-stable-id-view (ensembl-class)
  "Define an CLSQL view class for a *_stable_id table in EnsEMBL."
  (with-symbol-prefixing-function (table-concat ensembl-class)
    `(progn (def-ensembl-view ,(table-concat '-stable-id) ()
              ((,(table-concat '-ID)
                 :type integer
                 :db-type :key
                 :db-constraints :not-null)
               (stable-id
                :type (string 128)
                :db-type :key
                :db-constraints :not-null)
               (version :type integer)
               (created-date :type string)
               (modified-date :type string)
               (table-name
                :db-kind :join
                :db-info (:join-class ,(table-concat)
                          :home-key ,(table-concat '-ID)
                          :foreign-key ,(table-concat '-ID)
                          :set nil)))))))


(def-ensembl-view dna-sequence ()
  ((seq-region-id :type integer
                  :db-constraints :not-null
                  :initarg :seq-region-id)
   (seq-region-start :type integer
                     :db-constraints :not-null
                     :initarg :seq-region-start)
   (seq-region-end :type integer
                   :db-constraints :not-null
                   :initarg :seq-region-end)
   (seq-region-strand :type integer
                      :db-constraints :not-null
                      :initarg :seq-region-strand)
   (seq-region :accessor seq-region
               :db-kind :join
               :db-info (:join-class seq-region
                         :home-key seq-region-id
                         :foreign-key seq-region-id
                         :set nil))
   (previous-element-fn :accessor previous-element-fn
                        :initform #'1-
                        :db-kind :virtual
                        :allocation :class)
   (next-element-fn :accessor next-element-fn
                    :initform #'1+
                    :allocation :class
                    :db-kind :virtual)
   (element-member-fn :accessor element-member-fn
                      :initform
                      #'(lambda (set x)
                          (with-slots (seq-region-start seq-region-end) set
                            (and (integerp x)
                                 (<= seq-region-start x seq-region-end))))
                      :allocation :class
                      :db-kind :virtual)))


(defmacro def-ensembl-class (class-name slots &key dna-sequence stable-id-char)
  (with-symbol-prefixing-function (table-concat class-name)
    `(progn
       ,(when stable-id-char
              `(progn
                (def-ensembl-stable-id-view ,class-name)
                (def-fetch-by-stable-id-method ,class-name)
                (push (cons ,stable-id-char ',class-name) *char-class-alist*)))
       (def-ensembl-view ,class-name (,@(when dna-sequence '(dna-sequence)))
          ,(append ;; If the object has a stable id, we need to link
                   ;; to the appropriate table.
                   (when stable-id-char
                     `((stable-id
                        :db-kind :join
                        :db-info (:join-class ,(table-concat "-STABLE-ID")
                                  :home-key ,(table-concat "-ID")
                                  :foreign-key ,(table-concat "-ID")
                                  :set nil))))
                   ;; The slot definitions for the table.
                   slots)))))



;;;; --------------------------------------------------------------------------
;;;; Class definitions
;;;; -------------------------------------------------------------------------

;;; Coordinate Systems and Sequences
;;; --------------------------------
(def-ensembl-class assembly
  ((asm-seq-region-id :type integer
                      :db-kind :key
                      :db-constraints :not-null)
   (cmp-seq-region-id :type integer
                      :db-kind :key
                      :db-constraints :not-null)
   (asm-start :type integer
              :db-kind :key
              :db-constraints :not-null)
   (asm-end :type integer
            :db-kind :key
            :db-constraints :not-null)
   (cmp-start :type integer
              :db-kind :key
              :db-constraints :not-null)
   (cmp-end :type integer
            :db-kind :key
            :db-constraints :not-null)
   (ori :type integer
        :db-kind :key
        :db-constraints :not-null)
   (asm-seq-region :accessor asm-seq-region
                   :db-kind :join
                   :db-info (:join-class seq-region
                             :home-key asm-seq-region-id
                             :foreign-key seq-region-id
                             :set nil))
   (cmp-seq-region :accessor cmp-seq-region
                   :db-kind :join
                   :db-info (:join-class seq-region
                             :home-key cmp-seq-region-id
                             :foreign-key seq-region-id
                             :set nil))))


(def-ensembl-view seq-region ()
  ((seq-region-id :type integer
                  :db-kind :key
                  :db-constraints (:primary-key :not-null :unsigned))
   (name :type (varchar 40))
   (coord-system-id :type integer)
   (length :type integer)
   (coord-system :accessor coord-system
                 :db-kind :join
                 :db-info (:join-class coord-system
                           :home-key coord-system-id
                           :foreign-key coord-system-id
                           :set nil))
   (assembly :accessor assembly
             :db-kind :join
             :db-info (:join-class assembly
                       :home-key seq-region-id
                       :foreign-key asm-seq-region-id))
   (cmp-assembly :accessor cmp-assembly
                 :db-kind :join
                 :db-info (:join-class assembly
                           :home-key seq-region-id
                           :foreign-key cmp-seq-region-id))))


(def-ensembl-view coord-system ()
  ((coord-system-id :type integer
                    :db-kind :key
                    :db-constraints (:primary-key :not-null :unsigned))
   (name :type (varchar 40)
         :db-constraints :not-null)
   (version :type (varchar 40))
   (rank :type integer :db-constraints :not-null)
   (attrib :type string #||set('default_version','sequence_level')||#)))


;;; Sequence tables
;;; ---------------
(def-ensembl-view dna ()
  ((seq-region-id :type integer
                  :db-kind :key
                  :db-constraints (:primary-key :not-null :unsigned))
   (sequence :type string)))



;;; --------------------------------------------------------------------------
;;; Sequence object representations
;;; --------------------------------------------------------------------------

;;; DNA and RNA
;;; -----------
(def-ensembl-class transcript
  ((transcript-id  :type integer
                   :db-kind :key
                   :db-constraints (:primary-key :not-null :unsigned)
                   :initarg :transcript-id)
   (gene-id :type integer
            :db-constraints :not-null)
   (analysis-id :type integer
                :db-constraints :not-null)
   (display-xref-id :type integer
                    :accessor display-xref-id)
   (biotype :type (varchar 40))
   (status :type string
           :accessor transcript-status
           :documentation "One of
'KNOWN','NOVEL','PUTATIVE','PREDICTED','KNOWN_BY_PROJECTION','UNKNOWN'")
   (description :type string)
   (is-current :type integer)
   (gene :db-kind :join
         :db-info (:join-class gene
                               :home-key gene-id
                               :foreign-key gene-id
                               :set nil))
   (exon-transcript :db-kind :join
                    :db-info (:join-class exon-transcript
                              :home-key transcript-id
                              :foreign-key transcript-id
                              :set t))
   (translation :db-kind :join
                :db-info (:join-class translation
                          :home-key transcript-id
                          :foreign-key transcript-id
                          :set nil)))
  :dna-sequence t
  :stable-id-char #\t)


(def-ensembl-class exon
  ((exon-id :type integer
            :db-type :key
            :db-constraints (:primary-key :not-null))
   (phase :type integer
          :accessor exon-phase)
   (end-phase :type integer
              :accessor exon-end-phase)
   (is-current :type integer)
   (exon-transcript :db-kind :join
                    :db-info (:join-class exon-transcript
                              :home-key exon-id
                              :foreign-key exon-id
                              :set nil)))
  :stable-id-char #\e
  :dna-sequence t)


(def-ensembl-view exon-transcript ()
  ((exon-id :type integer
            :db-type :key
            :db-constraints (:primary-key :not-null))
   (transcript-id :type integer 
                  :db-type :key
                  :db-constraints :not-null)
   (rank :type integer
         :db-type :key
         :db-constraints :not-null)
   (exon :db-kind :join
         :db-info (:join-class exon
                   :home-key exon-id
                   :foreign-key exon-id
                   :set t))
   (transcript :db-kind :join
               :db-info (:join-class transcript
                         :home-key transcript-id
                         :foreign-key transcript-id
                         :set nil))))


(def-ensembl-class gene
  ((gene-id :type integer
            :db-type :key
            :db-constraints (:primary-key :not-null))
   (biotype :type (string 40) :db-info (:retrieval deferred))
   (analysis-id :type integer :db-info (:retrieval deferred))
   (display-xref-id :type integer)
   (source :type (string 20) :db-info (:retrieval deferred))
   (status :type string :db-info (:retrieval deferred))
   (description :type string)
   (is-current :type integer :db-info (:retrieval deferred))
   (transcript :db-kind :join
               :db-info (:join-class transcript
                         :home-key gene-id
                         :foreign-key gene-id
                         :set t)))
  :stable-id-char #\g
  :dna-sequence t)

;;; --------------------------------------------------------------------------
;;; Proteins and Protein Features
;;; --------------------------------------------------------------------------
(def-ensembl-view protein-feature ()
  ((protein-feature-id :type integer :db-constraints (:not-null :primary-key)
                       :db-kind :key)
   (translation-id :type integer     :db-constraints (:not-null))
   (seq-start :type integer)
   (seq-end   :type integer)
   (hit-start :type integer)
   (hit-end   :type integer)
   (analysis-id :type integer)
   (score       :type float :db-info (:retrieval deferred))
   (evalue      :type float :db-info (:retrieval deferred))
   (perc-ident  :type float)
   (analysis  :db-kind :join
              :db-info (:join-class analysis
                        :home-key analysis-id
                        :foreign-key analysis-id
                        :set nil)
              :accessor analysis)
   (translation :db-kind :join
                :db-info (:join-class translation
                          :home-key translation-id
                          :foreign-key translation-id
                          :set nil))))

(def-ensembl-view analysis ()
  ((analysis-id :type integer :db-constraints (:not-null :primary-key)
                :db-kind :key)
   (created     :type string)
   (db         :type (varchar 120))
   (db-version :type (varchar 40))
   (db-file    :type (varchar 120))
   (program         :type (varchar 80))
   (program-version :type (varchar 40))
   (program-file    :type (varchar 80))
   (parameters      :type (varchar 255))
   (module          :type (varchar 80))
   (module-version  :type (varchar 40))
   (gff-source      :type (varchar 40))
   (gff-feature     :type (varchar 40))))

(def-ensembl-class translation
  ((translation-id :type integer :db-constraints (:not-null :primary-key)
                   :db-kind :key)
   (transcript-id  :type integer :db-constraints (:not-null :unsigned))
   (seq-start :type integer      :db-constraints :not-null)
   (seq-end   :type integer      :db-constraints :not-null)
   (start-exon-id :type integer  :db-constraints (:unsigned :not-null))
   (end-exon-id   :type integer  :db-constraints (:unsigned :not-null))
   (start-exon :accessor translation-start-exon
               :db-kind :join
               :db-info (:join-class exon
                         :home-key start-exon-id
                         :foreign-key exon-id
                         :set nil))
   (end-exon :accessor translation-end-exon
             :db-kind :join
             :db-info (:join-class exon
                       :home-key end-exon-id
                       :foreign-key exon-id
                       :set nil))
   (transcript :db-kind :join
               :db-info (:join-class transcript
                         :home-key transcript-id
                         :foreign-key transcript-id
                         :set nil))
   (protein-feature :db-kind :join
                    :db-info (:join-class protein-feature
                              :home-key translation-id
                              :foreign-key translation-id)
                    :accessor protein-feature))
  :stable-id-char #\p)



;;; --------------------------------------------------------------------------
;;; Other stuff
(def-ensembl-view meta ()
  ((meta-id
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :meta-id)
   (meta-key
    :accessor meta-key
    :type (string 40)
    :initarg :meta-key)
   (meta-value
    :accessor meta-value
    :type (string 255)
    :initarg :meta-value)))


;(locally-enable-sql-reader-syntax)


