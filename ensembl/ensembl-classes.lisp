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

(defmacro def-ensembl-stable-id-view (ensembl-class &rest class-args)
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
                          :set nil)))
              ,@class-args))))


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
                      :db-kind :virtual))
  (:documentation "Represents genomic sequences. The data has reached
  a stage such that the DNA sequence can indeed be represented in form
  of a chromosomal block. Though, for some organisms the DNA sequence
  may not yet be consecutive. If you are interested to learn about all
  the fractions of the genome that have been sequenced then you want
  to inspect the repository of traces that Ensembl provides
  separatedly."))


(defmacro def-ensembl-class (class-name slots &rest class-args)
  (flet ((class-arg-keyword (keyword)
           (prog1
               (cadr (find keyword class-args :key #'car))
             (setf class-args (remove keyword class-args :key #'car)))))
    (let ((stable-id-char (class-arg-keyword :stable-id-char))
          (dna-sequence   (class-arg-keyword :dna-sequence)))
      (with-symbol-prefixing-function (table-concat class-name)
        `(progn
           ,(when stable-id-char
                  `(progn
                     (def-ensembl-stable-id-view ,class-name)
                     (def-fetch-by-stable-id-method ,class-name)
                     (push (cons ,stable-id-char ',class-name)
                           *char-class-alist*)))
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
               slots)
             ,@class-args))))))



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
                           :foreign-key cmp-seq-region-id))
   (dna-align-feature :db-kind join
		      :db-info (:join-class dna-align-feature
				:foreign-key seq-region-id
				:home-key seq-region-id)))
  (:base-table "seq_region")
  (:documentation "The same physical basepair may be described in multiple sequence regions of this table. "))


(def-ensembl-view coord-system ()
  ((coord-system-id :type integer
                    :db-kind :key
                    :db-constraints (:primary-key :not-null :unsigned))
   (name :type (varchar 40)
         :db-constraints :not-null)
   (version :type (varchar 40))
   (rank :type integer :db-constraints :not-null)
   (attrib :type string #||set('default_version','sequence_level')||#)))

(def-ensembl-view attrib-type ()
  ((attrib-type-id    :db-kind :key  :type integer)
   (code              :db-kind :key  :type (string 15))
   (name              :db-kind :base :type (string 255))
   (description       :db-kind :base :type string)
   (seq-region-attrib :db-kind :join
		      :db-info (:join-class seq-region-attrib
                                :home-key attrib-type-id
				:foreign-key attrib-type-id)))
  (:base-table "attrib_type")
  (:documentation "The attributes are assigned to sequence regions and
  inform about various properties that a particular sequence region
  may have, e.g., the number of micro RNAs or alternative IDs."))


(def-ensembl-view seq-region-attrib ()
  ((seq-region-id  :db-kind :key :type integer)
   (attrib-type-id :db-kind :key :type integer)
   (value          :db-kind :key :type string) 
   (attrib-type    :db-kind :join
		   :db-info (:join-class  attrib-type
		             :home-key    attrib-type-id
			     :foreign-key attrib-type-id))
   (seq-region     :db-kind :join
		   :db-info (:join-class  seq-region
			     :home-key    seq-region-id
			     :foreign-key seq-region-id)))
  (:base-table "seq_region_attrib")
  (:documentation "Additional information on the sequence region at hand."))

;;; Sequence tables
;;; ---------------
(def-ensembl-view dna ()
  ((seq-region-id :type integer
                  :db-kind :key
                  :db-constraints (:primary-key :not-null :unsigned))
   (sequence :type string)))

(def-ensembl-view dna-align-feature ()
  ((dna-align-feature-id :db-kind :key :type integer)
   (seq-region-id        :db-kind :key :type integer)
   (seq-region-start     :db-kind :base :type integer)
   (seq-region-end       :db-kind :base :type integer)
   (seq-region-strand    :db-kind :base :type integer)
   (hit-start            :db-kind :base :type integer)
   (hit-end              :db-kind :base :type integer)
   (hit-strand           :db-kind :base :type integer)
   (hit-name             :db-kind :key  :type (string 40))
   (analysis-id          :db-kind :key  :type integer)
   (score                :db-kind :base :type float)
   (evalue               :db-kind :base :type float)
   (perc-ident           :db-kind :base :type float)
  ;(cigar-line           :db-kind :base :type string)
   (external-db-id       :db-kind :key  :type integer)
   (hcoverage            :db-kind :base :type integer)
   (analysis             :db-kind :join
			 :db-info (:join-class analysis
                                   :foreign-key analysis-id
				   :home-key analysis-id
				   :retrieval :deferred))
   (external-db          :db-kind :join
			 :db-info (:join-class  external-db
                                   :home-key    external-db-id
                                   :foreign-key external-db-id))
   (seq-region           :db-kind :join
			 :db-info (:join-class  seq-region
				   :home-key    seq-region-id
				   :foreign-key seq-region-id)))
  (:base-table "dna_align_feature")
  (:documentation "All segments are investigated for sequence
  similarity with other known sequence databases. The comparison is
  identified by the hit-name and the entry in the
  external-db-id. Sadly, this information is not available to compare
  an organism with itself.

+----------------+--------------+
| external_db_id | db_name      |
+----------------+--------------+
|            700 | EMBL         | 
|           1800 | RefSeq_dna   | 
|           1820 | RefSeq_rna   | 
|           3300 | miRBase      | 
|           3800 | CCDS         | 
|           4100 | UniGene      | 
|           4200 | RFAM         | 
|           7200 | IMGT/LIGM_DB | 
+----------------+--------------+
"))

(def-ensembl-view external-db ()
  ((external_db_id :db-kind :key :type integer)
   (db_name                :type (string 28))
   (db_release             :type (string 255))
   (status                 :type (string 15))
   (dbprimary_acc_linkable :type integer)
   (display_label_linkable :type integer)
   (priority               :type integer)
   (db_display_name        :type (string 255))
   (type                   :type (string 25))
   (secondary_db_name      :type (string 255))
   (secondary_db_table     :type (string 255))
   (dna-align-feature :db-kind :join
		      :db-info ()))
   (:base-table "external_db")
   (:documentation ""))


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
   (analysis             :db-kind :join
			 :db-info (:join-class analysis
                                   :foreign-key analysis-id
				   :home-key analysis-id
				   :retrieval :deferred))
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
  (:dna-sequence t)
  (:stable-id-char #\t))


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
  (:stable-id-char #\e)
  (:dna-sequence t))


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
   (biotype :type (string 40))
   (analysis-id :type integer)
   (display-xref-id :type integer)
   (source :type (string 20))
   (status :type string)
   (description :type string)
   (is-current :type integer)
   (analysis   :db-kind :join
	       :db-info (:join-class analysis
			 :home-key analysis-id
			 :foreign-key analysis-id
			 :retrieval :deferred))
   (transcript :db-kind :join
               :db-info (:join-class transcript
                         :home-key gene-id
                         :foreign-key gene-id
                         :set t)))
  (:stable-id-char #\g)
  (:dna-sequence t)
  (:documentation "Segment of genomic or mitochondrial DNA that is
coding for RNA or protein."))

;;; --------------------------------------------------------------------------
;;; Proteins and Protein Features
;;; --------------------------------------------------------------------------
(def-ensembl-view protein-feature ()
  ((protein-feature-id :type integer :db-constraints (:not-null :primary-key)
                       :db-kind :key)
   (translation-id :type integer     :db-constraints (:not-null))
   (seq-start   :type integer)
   (seq-end     :type integer)
   (hit-start   :type integer)
   (hit-end     :type integer)
   (analysis-id :type integer)
   (score       :type float)
   (evalue      :type float)
   (perc-ident  :type float)
   (analysis    :db-kind :join
                :db-info (:join-class analysis
                          :home-key analysis-id
                          :foreign-key analysis-id
                          :set nil)
                :accessor analysis)
   (translation :db-kind :join
                :db-info (:join-class translation
                          :home-key translation-id
                          :foreign-key translation-id
                          :set nil)))
  (:documentation "Description of a segment of a protein."))

(def-ensembl-view analysis ()
  ((analysis-id     :type integer 
                    :db-constraints (:not-null :primary-key)
                    :db-kind :key)
   (created         :type string)
   (db              :type (varchar 120))
   (db-version      :type (varchar 40))
   (db-file         :type (varchar 120))
   (program         :type (varchar 80))
   (program-version :type (varchar 40))
   (program-file    :type (varchar 80))
   (parameters      :type (varchar 255))
   (module          :type (varchar 80))
   (module-version  :type (varchar 40))
   (gff-source      :type (varchar 40))
   (gff-feature     :type (varchar 40))
   (analysis-description :db-kind :join
			 :db-info (:join-class  analysis-description
				   :home-key    analysis-id
				   :foreign-key analysis-id)))
  (:base-table "analysis")
  (:documentation "This class is referred to from most of the other tables. It indicates that some result was yielded by an automatism. Since only the DNA sequence itself is produced in the lab and everything else is indeed produced by machines, the table analysis gives insights in the provenance of the information given."))

(def-ensembl-view analysis-description ()
  ((analysis-id :db-kind :key  :type integer)
   (description :db-kind :base :type string)
   (display-label :db-kind :base :type string)
   (displayable :db-kind :base :type integer)
   (web-data :db-kind :base :type string)
   (analysis :db-kind :join
	     :db-info (:join-class analysis
		       :home-key analysis-id
		       :foreign-key analysis-id)))
  (:base-table "analysis_description")
  (:documentation "Human-understandable explanation about the purpose of a particular analysis."))


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
  (:stable-id-char #\p)
  (:documentation "Translation is the product of a transcript. There
  is only one peptide per mRNA, which may be biologically bogus due to
  alternative translation initiation."))



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


